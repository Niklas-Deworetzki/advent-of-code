package aoc.utils

import java.security.MessageDigest

object Crypto {

  private val MD5 = MessageDigest.getInstance("MD5")

  private final val hexChars = "0123456789abcdef".toCharArray

  private final def arrayToHexString(bytes: Array[Byte]): String = {
    val chars: Array[Char] = new Array[Char](bytes.length * 2)
    for (index <- bytes.indices) {
      chars(index * 2) = hexChars((bytes(index) >> 4) & 0xF)
      chars(index * 2 + 1) = hexChars(bytes(index) & 0xF)
    }
    String.valueOf(chars)
  }


  def genMD5(transform: Int => String, start: Int = 0): LazyList[(Int, String)] =
    for (number <- LazyList.from(start))
      yield (number, arrayToHexString(MD5.digest(transform(number).getBytes)))

}
