import scala.annotation.tailrec
object Zipper:
   // A zipper for a binary tree.

   case class Zipper[A](
       focus: BinTree[A],
       up: Option[Zipper[A]] = None
   )

   // Get a zipper focussed on the root node.
   def fromTree[A](bt: BinTree[A]): Zipper[A] = Zipper(bt)

   // Get the complete tree from a zipper.
   @tailrec
   def toTree[A](zipper: Zipper[A]): BinTree[A] = zipper.up match
      case Some(value) => toTree(value)
      case None        => zipper.focus

   // Get the value of the focus node.
   def value[A](zipper: Zipper[A]): A = zipper.focus.value

   // Get the left child of the focus node, if any.
   def left[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper.focus.left match
      case Some(value) => Some(Zipper(value, Some(zipper)))
      case None        => None

   // Get the right child of the focus node, if any.
   def right[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper.focus.right match
      case Some(value) => Some(Zipper(value, Some(zipper)))
      case None        => None

   // Get the parent of the focus node, if any.
   def up[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper.up

   // Set the value of the focus node.
   def setValue[A](v: A, zipper: Zipper[A]): Zipper[A] = updateParent(zipper, zipper.focus.copy(value = v))

   // Replace a left child tree.
   def setLeft[A](l: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] =
      updateParent(zipper, zipper.focus.copy(left = l))

   // Replace a right child tree.
   def setRight[A](r: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] =
      updateParent(zipper, zipper.focus.copy(right = r))

   private def updateParent[A](z: Zipper[A], bt: BinTree[A]): Zipper[A] = z.up match
      case Some(value) => Zipper(bt, updateChild(value, z.focus, bt))
      case None        => Zipper(bt, None)

   private def updateChild[A](z: Zipper[A], ch: BinTree[A], newCh: BinTree[A]): Option[Zipper[A]] =
      val (l, r) = (left(z), right(z))
      if l.isDefined && l.get.focus == ch then
         val newF = z.focus.copy(left = Some(newCh))
         Some(Zipper(newF, Some(updateParent(z, newF))))
      else if r.isDefined && r.get.focus == ch then
         val newF = z.focus.copy(right = Some(newCh))
         Some(Zipper(newF, Some(updateParent(z, newF))))
      else None

// A binary tree.
case class BinTree[A](value: A, left: Option[BinTree[A]], right: Option[BinTree[A]])
