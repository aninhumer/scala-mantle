package aninhumer.mantle

sealed trait Expr
case class Lit(value: Int) extends Expr
case class Var(name: String) extends Expr

trait Interface[Ifc, FlipIfc] {
  def bindInterface(left: Ifc, right: FlipIfc): Map[String, Expr]
  def newInterface(name: String): (Ifc, FlipIfc)
}
object Interface {
  def invert[Ifc, FlipIfc](ifc: Interface[Ifc, FlipIfc]) = new Interface[FlipIfc, Ifc] {
    override def bindInterface(left: FlipIfc, right: Ifc): Map[String, Expr] =
      ifc.bindInterface(right, left)
    override def newInterface(name: String): (FlipIfc, Ifc) =
      ifc.newInterface(name).swap
  }

  def newInterface[Ifc, FlipIfc](
      name: String
  )(implicit ifc: Interface[Ifc, FlipIfc]): (Ifc, FlipIfc) =
    ifc.newInterface(name)

  implicit class InfixBind[Ifc, FlipIfc](left: Ifc)(implicit ifc: Interface[Ifc, FlipIfc]) {
    def :=(right: FlipIfc): Map[String, Expr] = ifc.bindInterface(left, right)
  }
}

sealed trait Signal
case class Input(name: String) extends Signal
case class Output(value: Expr) extends Signal

object Signal {
  implicit val InputInterface = new Interface[Input, Output] {
    override def bindInterface(left: Input, right: Output): Map[String, Expr] =
      Map(left.name -> right.value)
    override def newInterface(name: String): (Input, Output) =
      (Input(name), Output(Var(name)))
  }
  implicit val OutputInterface: Interface[Output, Input] =
    Interface.invert(InputInterface)
}

case class Pair[A, B](valueA: A, valueB: B)

object Pair {
  import Interface._
  implicit def PairInterface[InA, OutA, InB, OutB](
      implicit ifcA: Interface[InA, OutA],
      ifcB: Interface[InB, OutB]
  ) = new Interface[Pair[InA, InB], Pair[OutA, OutB]] {
    override def bindInterface(
        left: Pair[InA, InB],
        right: Pair[OutA, OutB]
    ): Map[String, Expr] =
      (left.valueA := right.valueA) ++ (left.valueB := right.valueB)

    override def newInterface(name: String): (Pair[InA, InB], Pair[OutA, OutB]) = {
      val (inA, outA) = ifcA.newInterface(name + "_Pair_A")
      val (inB, outB) = ifcB.newInterface(name + "_Pair_B")
      (Pair(inA, inB), Pair(outA, outB))
    }
  }
}
