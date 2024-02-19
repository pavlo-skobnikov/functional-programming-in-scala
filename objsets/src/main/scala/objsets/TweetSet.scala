package objsets

import TweetReader.*

/** A class to represent tweets.
  */
class Tweet(val user: String, val text: String, val retweets: Int):
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"

/** This represents a set of objects of type `Tweet` in the form of a binary
  * search tree. Every branch in the tree has two children (two `TweetSet`s).
  * There is an invariant which always holds: for every branch `b`, all elements
  * in the left subtree are smaller than the tweet at `b`. The elements in the
  * right subtree are larger.
  *
  * Note that the above structure requires us to be able to compare two tweets
  * (we need to be able to say which of two tweets is larger, or if they are
  * equal). In this implementation, the equality / order of tweets is based on
  * the tweet's text (see `def incl`). Hence, a `TweetSet` could not contain two
  * tweets with the same text from different users.
  *
  * The advantage of representing sets as binary search trees is that the
  * elements of the set can be found quickly. If you want to learn more you can
  * take a look at the Wikipedia page [1], but this is not necessary in order to
  * solve this assignment.
  *
  * [1] http://en.wikipedia.org/wiki/Binary_search_tree
  */
abstract class TweetSet extends TweetSetInterface:

  /** This method takes a predicate and returns a subset of all the elements in
    * the original set for which the predicate is true.
    */
  def filter(predicate: Tweet => Boolean): TweetSet =
    this.filterAcc(predicate, Empty())

  /** This is a helper method for `filter` that propagates the accumulated
    * tweets.
    */
  def filterAcc(predicate: Tweet => Boolean, acc: TweetSet): TweetSet

  /** Returns a new `TweetSet` that is the union of `TweetSet`s `this` and
    * `that`.
    *
    * Question: Should we implement this method here, or should it remain
    * abstract and be implemented in the subclasses?
    */
  def union(that: TweetSet): TweetSet =
    var accumulatedSet = this

    that.foreach(thatTweet => accumulatedSet = accumulatedSet.incl(thatTweet))

    accumulatedSet

  /** Returns the tweet from this set which has the greatest retweet count.
    *
    * Calling `mostRetweeted` on an empty set should throw an exception of type
    * `java.util.NoSuchElementException`.
    *
    * Question: Should we implement this method here, or should it remain
    * abstract and be implemented in the subclasses?
    */
  def mostRetweeted: Tweet = mostRetweetedOpt match
    case None =>
      throw java.util.NoSuchElementException("mostRetweeted of EmptySet")
    case Some(tweet) => tweet

  def mostRetweetedOpt: Option[Tweet]

  /** Returns a list containing all tweets of this set, sorted by retweet count
    * in descending order. In other words, the head of the resulting list should
    * have the highest retweet count.
    *
    * Hint: the method `remove` on TweetSet will be very useful. Question:
    * Should we implement this method here, or should it remain abstract and be
    * implemented in the subclasses?
    */
  def descendingByRetweet: TweetList =
    var accumulatedSet = this
    var tweetList: TweetList = Nil

    var mostRtOpt = accumulatedSet.mostRetweetedOpt

    while mostRtOpt.isDefined do
      val mostRt = mostRtOpt.get

      accumulatedSet = accumulatedSet.remove(mostRt)
      tweetList = tweetList.inclLast(mostRt)

      mostRtOpt = accumulatedSet.mostRetweetedOpt

    tweetList

  /** The following methods are already implemented
    */

  /** Returns a new `TweetSet` which contains all elements of this set, and the
    * the new element `tweet` in case it does not already exist in this set.
    *
    * If `this.contains(tweet)`, the current set is returned.
    */
  def incl(tweet: Tweet): TweetSet

  /** Returns a new `TweetSet` which excludes `tweet`.
    */
  def remove(tweet: Tweet): TweetSet

  /** Tests if `tweet` exists in this `TweetSet`.
    */
  def contains(tweet: Tweet): Boolean

  /** This method takes a function and applies it to every element in the set.
    */
  def foreach(func: Tweet => Unit): Unit

class Empty extends TweetSet:

  override def mostRetweetedOpt: Option[Tweet] = None

  override def filterAcc(predicate: Tweet => Boolean, acc: TweetSet): TweetSet =
    acc

  override def contains(tweet: Tweet): Boolean = false

  override def incl(tweet: Tweet): TweetSet = NonEmpty(tweet, Empty(), Empty())

  override def remove(tweet: Tweet): TweetSet = this

  override def foreach(func: Tweet => Unit): Unit = ()

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet:
  override def mostRetweetedOpt: Option[Tweet] =
    val mostRtLeft = left.mostRetweetedOpt
    val mostRtRight = right.mostRetweetedOpt

    val mostRtChild: Option[Tweet] = (mostRtLeft, mostRtRight) match
      case (None, None)        => None
      case (None, Some(tweet)) => Some(tweet)
      case (Some(tweet), None) => Some(tweet)
      case (Some(leftTweet), Some(rightTweet)) =>
        if leftTweet.retweets > rightTweet.retweets then Some(leftTweet)
        else Some(rightTweet)

    mostRtChild match
      case None => Some(elem)
      case Some(tweet) =>
        if tweet.retweets > elem.retweets then Some(tweet)
        else Some(elem)

  override def filterAcc(predicate: Tweet => Boolean, acc: TweetSet): TweetSet =
    val rightFiltered = right.filterAcc(predicate, acc)
    val leftFiltered = left.filterAcc(predicate, rightFiltered)

    if predicate(elem) then leftFiltered.incl(elem) else leftFiltered

  override def contains(tweet: Tweet): Boolean =
    if tweet.text < elem.text then left.contains(tweet)
    else if elem.text < tweet.text then right.contains(tweet)
    else true

  override def incl(tweet: Tweet): TweetSet =
    if tweet.text < elem.text then NonEmpty(elem, left.incl(tweet), right)
    else if elem.text < tweet.text then NonEmpty(elem, left, right.incl(tweet))
    else this

  override def remove(tweet: Tweet): TweetSet =
    if tweet.text < elem.text
    then NonEmpty(elem, left.remove(tweet), right)
    else if elem.text < tweet.text
    then NonEmpty(elem, left, right.remove(tweet))
    else left.union(right)

  override def foreach(func: Tweet => Unit): Unit =
    func(elem)
    left.foreach(func)
    right.foreach(func)

trait TweetList:
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def inclLast(tweet: Tweet): TweetList
  def foreach(func: Tweet => Unit): Unit =
    if !isEmpty then
      func(head)
      tail.foreach(func)

object Nil extends TweetList:
  override def head =
    throw java.util.NoSuchElementException("head of EmptyList")
  override def tail =
    throw java.util.NoSuchElementException("tail of EmptyList")
  override def isEmpty = true
  override def inclLast(tweet: Tweet): TweetList = Cons(tweet, Nil)

class Cons(val head: Tweet, val tail: TweetList) extends TweetList:
  override def isEmpty = false
  override def inclLast(tweet: Tweet): TweetList =
    Cons(head, tail.inclLast(tweet))

object GoogleVsApple:
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets
    .filter(tweet =>
      google.exists(matchedStr => tweet.text.contains(matchedStr))
    )
  lazy val appleTweets: TweetSet = TweetReader.allTweets
    .filter(tweet =>
      apple.exists(matchedStr => tweet.text.contains(matchedStr))
    )

  /** A list of all tweets mentioning a keyword from either apple or google,
    * sorted by the number of retweets.
    */
  lazy val trending: TweetList =
    googleTweets.union(appleTweets).descendingByRetweet

object Main extends App:
  // Print the trending tweets
  GoogleVsApple.trending.foreach(println)
