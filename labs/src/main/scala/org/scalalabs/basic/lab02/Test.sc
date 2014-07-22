import scala.collection.immutable
object ReplaceChars {

  def googleCodeJamGooglerese(lines: String*): Seq[String] = {
    //figure out missing chracter mapping
    println("Test2")
    val input = "ejp mysljylc kd kxveddknmc re jsicpdrysi rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd de kr kd eoya kw aej tysr re ujdr lkgc jv " filterNot (_ == ' ')
    val output = "our language is impossible to understand there are twenty six factorial possibilities so it is okay if you want to just give up" filterNot (_ == ' ')

    val alphabet = 'a' to 'z'

    //visualize missing chars in alphabet
    val existingCharsSorted = input.toSet.toList.sorted.mkString
    val visualMissingChars = alphabet.map(c => if (existingCharsSorted.contains(c)) c else ' ').mkString

  //compute mapping
    val initialMapping = (input zip output).toSet
    println(initialMapping)
    //ensure 1 to 1 mapping
    val check = initialMapping.groupBy(_._1).values.forall(_.size == 1)
    println(initialMapping)
    println(check)

    val mapper = immutable.Map('z' -> 'q', 'q' -> 'z', ' ' -> ' ')
    println("---")
//
//    lines.map(_ map mapper)
    Seq()
  }
  val in1 = "ejp mysljylc kd kxveddknmc re jsicpdrysi"
  googleCodeJamGooglerese(in1)
}