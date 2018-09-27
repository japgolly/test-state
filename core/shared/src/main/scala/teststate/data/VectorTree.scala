package teststate.data

import VectorTree.Node

final case class VectorTree[+A](elements: Vector[Node[A]]) extends AnyVal {
  def map[B](f: A => B): VectorTree[B] =
    VectorTree(elements.map(_.map(f)))

  def shallow: Vector[A] =
    elements.map(_.value)

  def iteratorWithLevel(currentLevel: Int = 0): Iterator[(Int, A)] =
    elements.iterator.flatMap(_.iteratorWithLevel(currentLevel))

  def prettyPrintedLines(indent: String = "  "): Iterator[String] =
    iteratorWithLevel().map(x => (indent * x._1) + x._2)
}

object VectorTree {
  val empty = apply(Vector.empty)

  final case class Node[+A](value: A, children: VectorTree[A]) {
    def map[B](f: A => B): Node[B] =
      Node(f(value), children.map(f))

    def iteratorWithLevel(currentLevel: Int = 0): Iterator[(Int, A)] =
      Iterator.single(currentLevel -> value) ++ children.iteratorWithLevel(currentLevel + 1)
  }

  def flat[A](as: Vector[A]): VectorTree[A] =
    apply(as.map(Node[A](_, empty)))

  def lift[A](a: Node[A]): VectorTree[A] =
    apply(Vector.empty[Node[A]] :+ a)

  def one[A](value: A): VectorTree[A] =
    one(value, empty)

  def one[A](value: A, children: VectorTree[A]): VectorTree[A] =
    lift(Node(value, children))
}