abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

def chars(tree: CodeTree): List[Char] = tree match{
    case Fork(_,_,chars,_)  => chars
    case Leaf(char,_)       => List(char)
}

def encode(tree: CodeTree)(text: List[Char]): List[Int] = {
  def encodeChar(tree: CodeTree, char: Char, acc: List[Int]): List[Int] = tree match {
    case Leaf(_,_)            => acc
    case Fork(left,right,_,_) => {
      if chars(left).contains(char) 
      then encodeChar(left,char,0 :: acc)
      else encodeChar(right,char,1 :: acc)
    }
  }
  text.foldLeft(Nil)( (acc: List[Int], x: Char) => acc ++ encodeChar(tree,x,Nil).reverse )
}