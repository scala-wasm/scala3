import scala.util.hashing.MurmurHash3

case class HashRepro(x: Int)

object MurmurHash3CaseClassHashRepro {
  def selectedPrefix(x: Product, caseClassName: String): String =
    if (caseClassName != null) caseClassName else x.productPrefix

  def main(args: Array[String]): Unit = {
    val value = HashRepro(1)

    val nullName: String = null
    val nonNullName: String = "HashRepro"

    // Exact shape used in MurmurHash3.caseClassHash:
    val hashWithNull = MurmurHash3.caseClassHash(value, nullName)
    val hashWithName = MurmurHash3.caseClassHash(value, nonNullName)

    val prefixFromNull = selectedPrefix(value, nullName)
    val prefixFromName = selectedPrefix(value, nonNullName)

    println(hashWithNull)
    println(hashWithName)
    println(prefixFromNull)
    println(prefixFromName)
  }
}
