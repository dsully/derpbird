package org.sully.derpbird

import java.io.IOException

import actors.Actor
import collection.mutable.HashMap

import twitter4j.{Status, TwitterException, TwitterFactory}

import org.apache.commons.configuration.HierarchicalINIConfiguration

import org.pircbotx.{MultiBotManager, PircBotX, UtilSSLSocketFactory}
import org.pircbotx.exception.{IrcException, NickAlreadyInUseException}
import org.pircbotx.hooks.{Listener, ListenerAdapter}
import org.pircbotx.hooks.events._
import org.pircbotx.hooks.managers.ThreadedListenerManager

class MyINIConfiguration(filename: String) extends HierarchicalINIConfiguration(filename) {

  // Override to not count '#' as a comment character.
  val COMMENT_CHARS = ";"

  // HierarchicalINIConfiguration doesn't actually split properly.
  override def getStringArray(key: String): Array[String] = {

    for (value:String <- getString(key).split(","))
      yield value.trim
  }
}

class Derpbird[T <: PircBotX] extends ListenerAdapter[T] with Listener[T] {

  var configs = new HashMap[String, HierarchicalINIConfiguration]

  // Regexes to match input from IRC.
  val sup_match = """^sup (.+)""".r
  val ext_match = """twitter.com/.+/statuses/(\d+)$""".r

  // Keep the config around so we can use it to rejoin channels on reconnect.
  def addConfig(server: String, config: HierarchicalINIConfiguration) {
    configs += server -> config
  }

  def joinChannels(event: ConnectEvent[T]) {

    val server = event.getBot.getServer
    val config = configs.get((server)).get

    for (channelSection <- config.getSection(server).getString("channels").split(",")) {
      val channelConfig = config.getSection(channelSection.trim)

      val channel  = channelConfig.getString("channel")
      val password = channelConfig.getString("password", null)

      println("Joining channel: " + channel + " on: " + server + ":" + event.getBot.getPort)
      event.getBot.joinChannel(channel, password)
    }
  }

  override def onConnect(event: ConnectEvent[T]) {
    println("Connected to: " + event.getBot.getServer + ":" + event.getBot.getPort)
    joinChannels(event)
  }

  // Re-connect to the server if we've been dropped.
  override def onDisconnect(event: DisconnectEvent[T]) {
    println("Disconnected from: " + event.getBot.getServer + ":" + event.getBot.getPort + ", attempting to reconnect in 5 seconds!")

    var attempts = 0

    Thread.sleep(5 * 1000)

    while (!event.getBot.isConnected && attempts < 15) {
      try {
        attempts += 1
        event.getBot.reconnect()
      } catch {
        case e: Exception => {
          Thread.sleep(10 * 1000)
        }
      }
    }
  }

  override def onKick(event: KickEvent[T]) {

    if (event.getRecipient.getNick == event.getBot.getNick) {
      println("Kicked from channel: " + event.getChannel.getName + ", sleeping for 5 seconds before rejoining..")
      Thread.sleep(5 * 1000)
      event.getBot.joinChannel(event.getChannel.getName)
    }
  }

  override def onMessage(event: MessageEvent[T]) {

    try {

      event.getMessage match {
        case sup_match(username) => TwitterFetch ! TwitterMessageFetchForUser(event, username)
        case ext_match(tweetId)  => TwitterFetch ! TwitterMessageFetchForId(event, tweetId)
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
case class TwitterMessageFetchForUser(event: MessageEvent[_ <: PircBotX], username: String)
case class TwitterMessageFetchForId(event: MessageEvent[_ <: PircBotX], id: String)

object TwitterFetch extends Actor {

  val twitter = new TwitterFactory().getInstance

  def formatStatus(status: Status) = "<" + status.getUser.getScreenName + "> " + status.getText

  def fetchTimeLineForUser(event: MessageEvent[_ <: PircBotX], username: String) {
    try {
      event.getBot.sendMessage(event.getChannel, formatStatus(twitter.getUserTimeline(username).get(0)))

    } catch {
      case te: TwitterException => {
        println("Failed to get timeline: " + te.getMessage)
      }
    }
  }

  def fetchTweet(event: MessageEvent[_ <: PircBotX], id: String) {
    try {
      event.getBot.sendMessage(event.getChannel, formatStatus(twitter.showStatus(id.toLong)))

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
        case TwitterMessageFetchForUser(event, username) => fetchTimeLineForUser(event, username)
        case TwitterMessageFetchForId(event, tweetId) => fetchTweet(event, tweetId)
        case _ => null
      }
    }
  }
}

object Main {

  def main(args: Array[String]) {

    if (args.length == 0) {
      println("Usage: <derpbird> config.ini")
      System.exit(1)
    }

    // Fire up the Actor that will send requests to Twitter.
    TwitterFetch.start()

    val config   = new MyINIConfiguration(args(0))
    val multi    = new MultiBotManager("Derpbird")
    val manager  = new ThreadedListenerManager
    val listener = new Derpbird

    // Associate our Derpbird class with the PircBotX manager.
    manager.addListener(listener)

    multi.setEncoding("UTF-8")
    multi.setListenerManager(manager)

    for (serverKey <- config.getStringArray("global.servers")) {

      val server   = config.getSection(serverKey)
      val host     = server.getString("host")
      val port     = server.getInt("port", 6667)
      val pass     = server.getString("password", null)
      val socket   = if (server.getBoolean("ssl", false)) new UtilSSLSocketFactory else null

      val bot      = multi.createBot(host, port, pass, socket)

      bot.setName(server.getString("nick", "derpbird"))
      bot.setLogin(server.getString("nick", "derpbird"))
      bot.setFinger(server.getString("finger", "herp derp"))
      bot.setVersion(server.getString("version", "Derpbird"))
      bot.setVerbose(server.getBoolean("verbose", true))

      // My identification? You don't need to see my identification.
      if (server.getString("nickpass", null) != null) {
        bot.identify(server.getString("nickpass"))
      }

      listener.addConfig(serverKey, config)
    }

    try {
      multi.connectAll()
    } catch {
      case e: (IOException, IrcException, NickAlreadyInUseException) => {
        println("Couldn't connect to all servers: " + e.toString)
      }
    }
  }
}
