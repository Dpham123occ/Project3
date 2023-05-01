object CommandAnalyzer {

  case class GameObj(desc: String, kind: String)

  val grammar: List[String] = List(
    "look",
    "inventory",
    "go {direction}",
    "climb {object}",
    "take {object}",
    "drop {object}",
    "examine {object}",
    "search {object}",
    "sit on {object}",
    "lie on {object}",
    "open {container}",
    "close {container}",
    "lock {container}",
    "unlock {container}",
    "put {item} in {container}",
    "put {item} on {supporter}",
    "wear {clothing}",
    "take off {clothing}",
    "tie {item} to {object}",
    "talk to {person}")

  val world: List[GameObj] = List(
    GameObj("north", "direction"),
    GameObj("south", "direction"),
    GameObj("east", "direction"),
    GameObj("west", "direction"),
    GameObj("comfy chair", "object"),
    GameObj("shabby twin bed", "object"),
    GameObj("elegant carpet", "object"),
    GameObj("soccer ball", "item"),
    GameObj("beach ball", "item"),
    GameObj("small green frog", "item"),
    GameObj("small tree frog", "item"),
    GameObj("large wooden box", "container"),
    GameObj("flimsy cardboard box", "container"),
    GameObj("solid wooden table", "supporter"),
    GameObj("glass side stand", "supporter"),
    GameObj("purple hoodie", "clothing"),
    GameObj("leather jacket", "clothing"),
    GameObj("very old man", "person"),
    GameObj("very young woman", "person"))

  // ==========================================================
  // helper functions
  // ==========================================================

  // OK
  // turn a string of words separated by spaces into a list of those words
  def getWordList(words: String): List[String] = words.split("\\s+").toList

  // ==========================================================
  // functions on world
  // ==========================================================

  // Note: All lists of string that are returned from the following methods should be distinct (no duplicates) and they
  // should be in sorted order.

  // OK
  // returns a list of all the adjectives (all but last words in desc) in world
  // example: cardboard, flimsy, very [ but not 'box' ]
  def getAdjectives: List[String] = world.flatMap(g => getWordList(g.desc).init).distinct.sorted

  // OK
  // returns a list of nouns (last words in desc) from world
  // example: north, bed, man
  def getNouns: List[String] = world.map(_.desc.split(" ").last).distinct.sorted

  // OK
  // example: direction, item, object
  def getGameObjectKinds: List[String] = world.map(_.kind).distinct.sorted

  // OK
  // returns a list of game-objects associated with a particular noun (no need to be sorted)
  // example: getGameObjects("frog") =>
  //   List(GameObj("small green frog", "item"), GameObj("small tree frog", "item"))
  def getGameObjects(noun: String): List[GameObj] = world.filter(gameObj => gameObj.desc.split("\\W+").contains(noun))

  // ==========================================================
  // functions on grammar
  // ==========================================================

  // Note: All lists of string that are returned from the following methods should be distinct (no duplicates) and they
  // should be in sorted order.

  // OK
  // returns a list of all verbs (first words) in grammar
  // example: look, examine, put
  def getVerbs: List[String] = grammar.flatMap(_.split(" ").headOption).distinct.sorted

  // Ok
  // returns a list of all prepositions in grammar
  // prepositions are words in grammar strings that are not verbs and not in curly braces
  // example: in, on, to
  def getPrepositions: List[String] = grammar.map(_.split("\\s+").tail.filterNot(word => word.startsWith("{") && word.endsWith("}")).mkString(" ")).filterNot(_.isEmpty).distinct.sorted

  // Ok
  // returns a list of all actions from grammar
  // actions are formed by combining the verb with its preposition (if any) with an underscore between them
  // example: look, examine, sit_on, put_in
  def getActions: List[String] = grammar.map(_.replaceAll("\\{.*?\\}", "").trim.replaceAll("\\s+", "_")).distinct.sorted

  // Ok
  // returns a list of game-object kinds from grammar
  // kinds are all the words in curly braces
  // example: direction, item, object
  def getGrammarObjectKinds: List[String] = grammar.flatMap(_.split("\\{").drop(1).map(_.takeWhile(_ != '}'))).distinct.sorted

  // OK
  // given a verb, returns a list of all grammar strings associated with it
  // example: getGrammarStrings("put") =>
  //   List("put {item} in {object}", "put {item} on {object}")
  def getGrammarStrings(verb: String): List[String] = grammar.filter(_.startsWith(verb)).distinct

  // ==========================================================
  // getVocab function
  // ==========================================================

  // Ok
  // returns a list of all known words (from grammar and world)
  // note: does not include words with curly braces
  // should include object, direction, etc., but not {object}, {direction}, etc.
  def getVocab: List[String] = (grammar.flatMap(_.split("\\W+").filterNot(_.startsWith("{"))) ++ world.map(_.desc.split("\\W+")).flatten).distinct.sorted

  // ==========================================================
  // command related function
  // ==========================================================

  // OK
  // return true if the words contain a preposition
  def hasPrep(words: String): Boolean = words.split("\\W+").exists(CommandAnalyzer.getPrepositions.contains)

  // OK
  // return true if the words match the specified game object
  // for a string of words to match a game object, all the words in the string
  // must be words contained in the description of the game object
  // example: wordsMatchGameObj("tree tree", GameObj("small tree frog", "item")) ==> true
  // example: wordsMatchGameObj("small ball", GameObj("beach ball", "item")) ==> false
  def wordsMatchGameObj(words: String, gameObj: GameObj): Boolean = words.split("\\W+").forall(gameObj.desc.split("\\W+").contains)

  // Ok
  // return true if the words (cmdWords) match the specified grammar (pattern).
  // Take "put {item} in {container}" as an example pattern. The first word in cmdWords must be "put". The following
  // word(s) must match the desc of an "item" kind GameObj based on wordsMatchGameObj function. The next word must be
  // "in". The ending word(s) must match the desc of a "container" kind GameObj based on wordsMatchGameObj function
  def wordsMatchPattern(cmdWords: String, pattern: String): Boolean = {
    val patternRegex = pattern.replaceAll("\\{\\w+\\}", "(\\\\S+(?:\\\\s\\\\S+)*)")
    val cmdRegex = s"^$patternRegex$$".r
    val cmdWordsArr = cmdWords.split("\\W+")
    cmdRegex.findFirstIn(cmdWords).exists { matchStr =>
      val matchWordsArr = matchStr.split("\\W+")
      (matchWordsArr zip cmdWordsArr).forall { case (matchWord, cmdWord) =>
        if (matchWord.startsWith("{") && matchWord.endsWith("}")) {
          val kind = matchWord.substring(1, matchWord.length - 1)
          world.exists(obj => obj.kind == kind && cmdWord.split("\\s+").forall(word => obj.desc.contains(word)))
        } else {
          matchWord == cmdWord
        }
      }
    }
  }
}