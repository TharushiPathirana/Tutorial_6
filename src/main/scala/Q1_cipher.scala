object Q1_cipher {
  def Encryption(text: String, shift: Int): String ={
    var answer = ""
    for (character <- text) {
      var newShift = shift%26
      val newCharacter = (character + newShift).toChar
      if (character >= 'a' && character <= 'z') {
        if(newCharacter <= 'a' || newCharacter >= 'z'){
          newShift = newShift -('z'-character)-1
          answer += ('a'+newShift).toChar
        }
        else answer += newCharacter
      }
      else if (character >= 'A' && character <= 'Z'){
        if (newCharacter <= 'A' || newCharacter >= 'Z') {
          newShift = newShift - ('Z' - character) - 1
          answer += ('A' + newShift).toChar
        }
        else answer += newCharacter
      }
      else answer += character
    }
    answer
  }

  def Decryption(text: String, shift: Int): String = {
    var answer = ""
    for (character <- text) {
      var newShift = shift % 26
      val newCharacter = (character - newShift).toChar
      if (character >= 'a' && character <= 'z') {
        if (newCharacter < 'a' || newCharacter > 'z') {
          newShift =  newShift - (character - 'a') -1
          answer += ('z' - newShift).toChar
        }
        else answer += newCharacter
      }
      else if (character >= 'A' && character <= 'Z') {
        if (newCharacter < 'A' || newCharacter > 'Z') {
          newShift = newShift - (character - 'A') - 1
          answer += ('Z' - newShift).toChar
        }
        else answer += newCharacter
      }
      else answer += character
    }
    answer
  }

  def Cipher(text: String, shift: Int, formatFunc: (String, Int) => String): String = {
    formatFunc(text, shift)
  }

  def main(args: Array[String]): Unit = {
    println(Cipher("Tharushi#", 100, Encryption))
    println(Cipher("Pdwnqode#", 100, Decryption))

  }
}