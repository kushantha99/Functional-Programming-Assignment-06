def encryption(text: String, shift: Int): String = {
  val letterCount = 26

  def encProcess(letter: Char): Char = {
    if (letter.isLetter) {
      val base = if (letter.isUpper) 'A' else 'a'
      val shiftedChar = (base + (letter - base + letterCount + 1) % letterCount).toChar
      shiftedChar
    } else {
      letter
    }
  }

  text.map(encProcess)
}

def decrypting(text: String, shift: Int): String = {
  val letterCount = 26

  def decProcess(letter: Char): Char = {
    if (letter.isLetter) {
      val base = if (letter.isUpper) 'A' else 'a'
      val shiftedChar = (base + (letter - base + letterCount - 1) % letterCount).toChar
      shiftedChar
    } else {
      letter
    }
  }

  text.map(decProcess)
}

def main(args: Array[String]): Unit = {
  println("Enter Text")
  val userInput = scala.io.StdIn.readLine()
  println("if you need to encrypt enter 'e' or need to decrypt enter 'd':")
  val encORdec = scala.io.StdIn.readLine()
  println("Enter shift value")
  val shift = scala.io.StdIn.readInt
  
  if (encORdec == "e") {
    val encryptedText = encryption(userInput,shift)
    println(s"Encrypted Text: $encryptedText")   
  } else if (encORdec == "d") {
    val decryptedText = decrypting(userInput,shift)
    println(s"Decrypted Text: $decryptedText")
  } else {
    println("Invalid choice! Please enter 'e' or 'd'.")
  }
}
