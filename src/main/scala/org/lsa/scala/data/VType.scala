package fr.edf.dco.ma.reflex.data

import scala.language.implicitConversions

sealed trait VType {
  def toJSon: String
  def toBJSon(tabs: Int): String
}

case class VString(s: String) extends VType {
  def toJSon: String = "" + '"' + s + '"'
  def toBJSon(tabs: Int): String = toJSon
}

case class VBoolean(b: Boolean) extends VType {
  def toJSon: String = if (b) "true" else "false"
  def toBJSon(tabs: Int): String = toJSon
}

case class VDouble(d: Double) extends VType {
  def toJSon: String = String.valueOf(d)
  def toBJSon(tabs: Int): String = toJSon
}

case class VInt(i: Int) extends VType {
  def toJSon: String = String.valueOf(i)
  def toBJSon(tabs: Int): String = toJSon
}

case class VLong(l: Long) extends VType {
  def toJSon: String = String.valueOf(l)
  def toBJSon(tabs: Int): String = toJSon
}

case class VShort(s: Short) extends VType {
  def toJSon: String = String.valueOf(s)
  def toBJSon(tabs: Int): String = toJSon
}

case class VList(l: List[VType] = Nil) extends VType {
  def append(v: VType) = new VList(l :+ v)
  def :+(v: VType) = new VList(l :+ v)

  def toJSon: String = l.map(_.toJSon).mkString("[", ", ", "]")
  override def toBJSon(tabs: Int): String = toJSon
}

case class KVPair(key: String, private val raw: VType) extends VType {
  val value = raw match {
    case s: KVSet => s
    case p: KVPair => KVSet(p)
    case v: VType => v
  }
  
  def toJSon: String = "" + '"' + key + '"' + ": " + value.toJSon
  def toBJSon(tabs: Int): String = "" + '"' + key + '"' + ": " + value.toBJSon(tabs)
}

case class KVSet(items: Set[KVPair] = Set.empty) extends VType {
  
  def append(kv: KVPair) = new KVSet(items + kv)
  def +(kv: KVPair) = new KVSet(items + kv)
  def append(kvs: KVSet) = new KVSet(items ++ kvs.items)
  def ++(kvs: KVSet) = new KVSet(items ++ kvs.items)

  def toJSon: String = items.map(_.toJSon).mkString("{", ", ", "}")
  override def toBJSon(tabs: Int): String = {
    val tstr = "  " * tabs
    val nstr = "  " * (tabs + 1)
    items.map(_.toBJSon(tabs + 1)).mkString("{\n" + nstr, ",\n" + nstr, "\n" + tstr + "}")
  }
}

object KVSet {
  
  def apply(kv1: KVPair) =
    new KVSet(Set(kv1))
  def apply(kv1: KVPair, kv2: KVPair) =
    new KVSet(Set(kv1, kv2))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair) =
    new KVSet(Set(kv1, kv2, kv3))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair, kv8: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7, kv8))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair, kv8: KVPair, kv9: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7, kv8, kv9))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair, kv8: KVPair, kv9: KVPair, kv10: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7, kv8, kv9, kv10))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair, kv8: KVPair, kv9: KVPair, kv10: KVPair,
            kv11: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7, kv8, kv9, kv10, kv11))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair, kv8: KVPair, kv9: KVPair, kv10: KVPair,
            kv11: KVPair, kv12: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7, kv8, kv9, kv10, kv11, kv12))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair, kv8: KVPair, kv9: KVPair, kv10: KVPair,
            kv11: KVPair, kv12: KVPair, kv13: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7, kv8, kv9, kv10, kv11, kv12, kv13))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair, kv8: KVPair, kv9: KVPair, kv10: KVPair,
            kv11: KVPair, kv12: KVPair, kv13: KVPair, kv14: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7, kv8, kv9, kv10, kv11, kv12, kv13, kv14))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair, kv8: KVPair, kv9: KVPair, kv10: KVPair,
            kv11: KVPair, kv12: KVPair, kv13: KVPair, kv14: KVPair, kv15: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7, kv8, kv9, kv10, kv11, kv12, kv13, kv14, kv15))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair, kv8: KVPair, kv9: KVPair, kv10: KVPair,
            kv11: KVPair, kv12: KVPair, kv13: KVPair, kv14: KVPair, kv15: KVPair,
            kv16: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7, kv8, kv9, kv10, kv11, kv12, kv13, kv14, kv15, kv16))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair, kv8: KVPair, kv9: KVPair, kv10: KVPair,
            kv11: KVPair, kv12: KVPair, kv13: KVPair, kv14: KVPair, kv15: KVPair,
            kv16: KVPair, kv17: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7, kv8, kv9, kv10, kv11, kv12, kv13, kv14, kv15, kv16, kv17))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair, kv8: KVPair, kv9: KVPair, kv10: KVPair,
            kv11: KVPair, kv12: KVPair, kv13: KVPair, kv14: KVPair, kv15: KVPair,
            kv16: KVPair, kv17: KVPair, kv18: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7, kv8, kv9, kv10, kv11, kv12, kv13, kv14, kv15, kv16, kv17, kv18))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair, kv8: KVPair, kv9: KVPair, kv10: KVPair,
            kv11: KVPair, kv12: KVPair, kv13: KVPair, kv14: KVPair, kv15: KVPair,
            kv16: KVPair, kv17: KVPair, kv18: KVPair, kv19: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7, kv8, kv9, kv10, kv11, kv12, kv13, kv14, kv15, kv16, kv17, kv18, kv19))
  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair, kv8: KVPair, kv9: KVPair, kv10: KVPair,
            kv11: KVPair, kv12: KVPair, kv13: KVPair, kv14: KVPair, kv15: KVPair,
            kv16: KVPair, kv17: KVPair, kv18: KVPair, kv19: KVPair, kv20: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7, kv8, kv9, kv10, kv11, kv12, kv13, kv14, kv15, kv16, kv17, kv18, kv19, kv20))

  def apply(kv1: KVPair, kv2: KVPair, kv3: KVPair, kv4: KVPair, kv5: KVPair,
            kv6: KVPair, kv7: KVPair, kv8: KVPair, kv9: KVPair, kv10: KVPair,
            kv11: KVPair, kv12: KVPair, kv13: KVPair, kv14: KVPair, kv15: KVPair,
            kv16: KVPair, kv17: KVPair, kv18: KVPair, kv19: KVPair, kv20: KVPair,
            kv21: KVPair, kv22: KVPair, kv23: KVPair, kv24: KVPair, kv25: KVPair) =
    new KVSet(Set(kv1, kv2, kv3, kv4, kv5, kv6, kv7, kv8, kv9, kv10, kv11, kv12, kv13, kv14, kv15, kv16, kv17, kv18, kv19, kv20, kv21, kv22, kv23, kv24, kv25))

}

object VTypeUtils {
  implicit def toVString(s: String) = new VString(s)
  implicit def toVBoolean(b: Boolean) = new VBoolean(b)
  implicit def toVDouble(d: Double) = new VDouble(d)
  implicit def toVInt(i: Int) = new VInt(i)
  implicit def toVLong(l: Long) = new VLong(l)
  implicit def toVShort(s: Short) = new VShort(s)
  implicit def toVList(l: List[VType]) = new VList(l)
}

object Test extends App {
  import VTypeUtils._

  val kv1 = KVPair("att1", "Hello, world!")
  val kv2 = KVPair("att2", true)
  val ob1 = KVSet(kv1, kv2)

  val kv3 = KVPair("att3", 1234567890L)
  val kv4 = KVPair("att4", 123456)
  val kv5 = KVPair("att5", 123)
  val ob2 = KVSet(kv3, kv4, kv5)
  
  val kv6 = VInt(8)
  val kv7 = KVPair("att7", List[VType](1, 2, 3))

  val ob3 = KVSet(KVPair("sob1", ob1), KVPair("sob2", ob2), KVPair("att3", kv6), kv7, KVPair("att4", kv7))

  println(ob3.toBJSon(0))
}