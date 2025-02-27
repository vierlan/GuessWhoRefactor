//Handles game state, manages remaining characters, and processes user actions (questions & guesses).
class GameLogic {

  var remainingCharacters: List[Character] = GameData.characters
  val secretCharacter: Character = remainingCharacters(scala.util.Random.nextInt(remainingCharacters.length))

  // Unique icons for each character
  val characterIcons: Map[String, String] = Map(
    "Ronaldo" -> "⚽", "April" -> "👩‍🎤", "Robyn" -> "👸", "Waris" -> "🕵️‍♂️",
    "Halaand" -> "⚽", "Dorothea" -> "📚", "Sandra" -> "🚀", "Mary" -> "🎭"
  )

  def askQuestion(question: String): List[Character] = {
    val (answer, newRemainingCharacters) = QuestionFilter.filterCharacters(remainingCharacters, question, secretCharacter)

    if (answer) {
      println(Console.GREEN + "✅ Yes!" + Console.RESET)
    } else {
      println(Console.RED + "❌ No." + Console.RESET)
    }

    remainingCharacters = newRemainingCharacters
    displayRemainingCharacters()
    remainingCharacters
  }

  def guessCharacter(name: String): Boolean = {
    val guessedCorrectly: Boolean = name.equalsIgnoreCase(secretCharacter.name)
    val characterNameList: List[String] = GameData.characters.map(char => char.name)
    val guessOnList: Boolean = characterNameList.contains(name.toLowerCase.capitalize)

    if (guessedCorrectly) {
      remainingCharacters = List(secretCharacter)
      println(Console.GREEN + s"🎉 Congratulations! You guessed correctly: $name" + Console.RESET)
    } else if (guessOnList) {
      remainingCharacters = remainingCharacters.filterNot(_.name.equalsIgnoreCase(name))
      println(Console.RED + s"❌ Incorrect guess: $name. Try asking more questions." + Console.RESET)
    } else println(Console.RED + s"❌👿 NO!🤬 $name is not a valid character!😤" + Console.RESET)

    displayRemainingCharacters()
    guessedCorrectly
  }

  def displayRemainingCharacters(): Unit = {
    println(Console.YELLOW + "\n📜 Remaining Characters:" + Console.RESET)

    remainingCharacters.foreach { character =>
      val icon = characterIcons.getOrElse(character.name, "❓") // Default if not found
      println(Console.BLUE + s"[ $icon ${character.name}]" + Console.RESET)
    }
  }
}












































