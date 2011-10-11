package org.sully

import org.apache.commons.configuration.{Configuration, PropertiesConfiguration, CompositeConfiguration}
import org.jibble.pircbot.PircBot
import twitter4j.Status
import twitter4j.TwitterException
import twitter4j.TwitterFactory

import actors.Actor

class Derpbird extends PircBot {

  var channels = collection.mutable.Set[String]()
  var nick     = ""
  var finger   = ""

  // Regexes to match input from IRC.
  val sup_match = """^sup (.+)""".r
  val ext_match = """twitter.com/.+/statuses/(\d+)$""".r

  def initialize() {
    TwitterFetch.start()

    setLogin(nick)
    setName(nick)
    setFinger(finger)
    //startIdentServer()
  }

  // Keep a set of channels to use on reconnect.
  def addChannel(channel: String) {
    channels += channel
  }

  def joinChannels() {
    channels.foreach(channel => println("onConnect: joining channel: " + channel))
    channels.foreach(channel => joinChannel(channel))
  }

  override def onConnect() {
    println("Connected to: " + getServer + ":" + getPort)
    println(channels)
    channels.foreach(channel => joinChannel(channel))
    joinChannels()
  }

  // Re-connect to the server if we've been dropped.
  override def onDisconnect() {
    println("Disconnected from: " + getServer + ":" + getPort + ", attempting to reconnect in 5 seconds!")

    var attempts = 0

    Thread.sleep(5 * 1000)

    while (!isConnected && attempts < 15) {
      try {
        attempts += 1
        reconnect()
      } catch {
        case e: Exception => {
          Thread.sleep(10 * 1000)
        }
      }
    }
  }

  override def onKick(channel: String, nick: String, login: String, hostname: String, kickee: String, reason: String) {

    if (kickee.equalsIgnoreCase(getNick)) {
      println("Kicked from channel: " + channel + ", sleeping for 5 seconds before rejoining..")
      Thread.sleep(5 * 1000)
      joinChannel(channel)
    }
  }

  override def onMessage(channel: String, sender: String, login: String, hostname: String, message: String) {

    try {

      message match {
        case sup_match(username) => TwitterFetch ! TwitterMessageFetchForUser(username, this, channel)
        case ext_match(tweetId)  => TwitterFetch ! TwitterMessageFetchForId(tweetId, this, channel)
        case _ => null
      }

    } catch {
      case e: Exception => {
        e.printStackTrace()
      }
    }
  }
}

// Actor matching.
case class TwitterMessageFetchForUser(username: String, bot: Derpbird, channel: String);
case class TwitterMessageFetchForId(id: String, bot: Derpbird, channel: String);

object TwitterFetch extends Actor {

  val twitter = new TwitterFactory().getInstance

  def formatStatus(status: Status) = "<" + status.getUser.getScreenName + "> " + status.getText

  def fetchTimeLineForUser(username: String, bot: Derpbird, channel: String) {
    try {
      bot.sendMessage(channel, formatStatus(twitter.getUserTimeline(username).get(0)))

    } catch {
      case te: TwitterException => {
        println("Failed to get timeline: " + te.getMessage)
      }
    }
  }

  def fetchTweet(id: String, bot: Derpbird, channel: String) {
    try {
      bot.sendMessage(channel, formatStatus(twitter.showStatus(id.toLong)))

    } catch {
      case te: TwitterException => {
        println("Failed to get timeline: " + te.getMessage)
      }
    }
  }

  def act() {
    twitter.verifyCredentials

    loop {
      react {
        case TwitterMessageFetchForUser(username, bot, channel) => fetchTimeLineForUser(username, bot, channel)
        case TwitterMessageFetchForId(tweetId, bot, channel) => fetchTweet(tweetId, bot, channel)
        case _ => null
      }
    }
  }
}

object Main {

  def main(args: Array[String]) {

    val config = new CompositeConfiguration()

    if (args.length > 0)
      config.append(new PropertiesConfiguration(args(0)))

    config.append(defaultConfiguration())

    val bot = new Derpbird()

    bot.setVerbose(config.getBoolean("verbose", true))
    bot.nick = config.getString("nick", "derpbird")
    bot.finger = config.getString("finger", "herp derp")

    for (channel <- config.getStringArray("channels")) {
      bot.addChannel(channel)
    }

    bot.initialize()

    try {
      bot.connect(config.getString("host"), config.getInt("port"))
    } catch {
      case e: java.net.ConnectException => {
        println("Couldn't connect to IRC Server: " + bot.getServer + ":" + bot.getPort)
      }
    }
  }

  private def defaultConfiguration() : Configuration = new PropertiesConfiguration(Main.getClass.getResource("default.properties"))
}
