package com.juanignaciosl.catvsdog

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.util.logging.Logged

class CatVsDogTest extends FlatSpec with ShouldMatchers {
  
  "(C1, D1) and (C1, D1)" should "be compatible" in {
    new Vote("C1", "D1").isCompatibleWith(new Vote("C1", "D1")) should be (true)
  }
  
  "(C1, D1) and (C2, D2)" should "be compatible" in {
    new Vote("C1", "D1").isCompatibleWith(new Vote("C2", "D2")) should be (true)
  }
  
  "(C1, D1) and (D2, C1)" should "not be compatible" in {
    new Vote("C1", "D1").isCompatibleWith(new Vote("D2", "C1")) should be (false)
  }
  
  "1 cat, 1 dog, 2 viewers with different vote" should "have 1 happy voters" in {
    val votes = new Vote("C1", "D1") :: new Vote("D1", "C1") :: Nil
    new CatVsDogTestCase(1, 1, 2, votes).maximumHappyVoters() should be (1)
  }
  
  "1 cat, 1 dog, 2 viewers with same vote" should "have 2 happy voters" in {
    val votes = new Vote("C1", "D1") :: new Vote("C1", "D1") :: Nil
    new CatVsDogTestCase(1, 1, 2, votes).maximumHappyVoters() should be (2)
  }
  
  "spotify first test case" should "have 1 happy voter" in {
    val votes = new Vote("C1", "D1") :: new Vote("D1", "C1") :: Nil
    new CatVsDogTestCase(1, 1, 2, votes).maximumHappyVoters() should be (1)
  }
  
  "spotify second test case" should "have 3 happy voters" in {
    val votes = new Vote("C1", "D1") :: new Vote("C1", "D1") :: new Vote("C1", "D2") :: new Vote("D2", "C1") :: Nil
    new CatVsDogTestCase(1, 2, 4, votes).maximumHappyVoters() should be (3)
  }
  
  "spotify second test case permutation" should "have 3 happy voters" in {
    val votes = new Vote("C1", "D1") :: new Vote("D2", "C1") :: new Vote("C1", "D1") :: new Vote("C1", "D2") :: Nil
    new CatVsDogTestCase(1, 2, 4, votes).maximumHappyVoters() should be (3)
  }
  
  "spotify second test case permutation variation" should "have 3 happy voters" in {
    val votes = new Vote("C1", "D1") :: new Vote("D2", "C1") :: new Vote("C1", "D1") :: new Vote("C1", "D2") :: new Vote("D2", "C1")  :: Nil
    new CatVsDogTestCase(1, 2, 5, votes).maximumHappyVoters() should be (3)
  }
  
}

class Vote(val stayer: String, val leaver: String) extends (String, String)(stayer, leaver) {
  def isCompatibleWith(other: Vote): Boolean = {
    return this == other || ( stayer != other.leaver && leaver != other.stayer )
  }
}

class CatVsDogTestCase(cats: Int, dogs: Int, voters: Int, votes: List[Vote]) {
  
  def maximumHappyVoters(): Int = {
    return maximumHappyVoters(votes)
  }
  
  private def maximumHappyVoters(votes: List[Vote]): Int = {
    val mostCompatibleVotes = removeLessCompatibleVoter(votes)
    if(votes.size == mostCompatibleVotes.size) {
      return votes.size
    } else {
      return maximumHappyVoters(votes.intersect(mostCompatibleVotes))
    }
  }
  
  def removeLessCompatibleVoter(votes: List[Vote]): List[Vote] = {
    var botheredVotersPerVoter = List[Int]()
    votes.foreach((evaluatedVoter: Vote) => {
      botheredVotersPerVoter :+= votes.filterNot((voter: Vote) => voter.isCompatibleWith(evaluatedVoter)).size
    });
    
    var maximumBothering = botheredVotersPerVoter.max
    if(maximumBothering == 0) {
      return votes
    } else {
      var maxBotheringVote = votes(botheredVotersPerVoter.indexOf(maximumBothering))
      return votes.diff(maxBotheringVote :: Nil)
    }
  }
}