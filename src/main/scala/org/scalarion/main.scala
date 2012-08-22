package org.scalarion

object Conversions{
  implicit def symbolToField(sym: Symbol) = new Field(sym)
}

abstract class FirstTerm{
  def equal(value: Any) = new ConditionalCriterion(this, new EqualCmp, value)
  def in[T](values: Seq[T]) = new ConditionalCriterion(this, new InCmp, values)
  def toSql :String
  def toPrepareSql: String
  def symbol: Symbol
}

class Field(sym: Symbol) extends FirstTerm{
  def toSql = "`" + sym.name + "`"
  def mutator(functionName: String) = new Mutator(functionName, this)
  def sum = mutator("SUM")
  def toPrepareSql = "{" + sym.name + "}"
  def symbol = sym
}

class Mutator(functionName: String, field: Field) extends FirstTerm{
  def toSql = functionName + "(" + field.toSql + ")";
  def toPrepareSql = functionName + "( "+ field.toPrepareSql +" )"
  def symbol = field.symbol
}

class ConditionalCriterion(firstTerm: FirstTerm, comparator: Comparator, value: Any) extends Criterion{
  def toSql = firstTerm.toSql + " " + comparator.toSql(value)
  def toPrepareSql = firstTerm.toSql + " " + comparator.toPrepareSql(firstTerm)
  def getParams = List(firstTerm.symbol -> value )
}

abstract class Criterion{
  def toSql: String
  def toPrepareSql: String
  def or(c2: Criterion) = new OrCriterion(this, c2)
  def and(c2: Criterion) = new AndCriterion(this, c2)
  def getParams: List[(Symbol,_)]
}

object Criterion{
  
  def apply(filters: Map[String, Any]): Criterion ={
    filters.filter{
      case (key,None) => false
      case _ => true
    }.map(t => new Field(Symbol(t._1)).equal(t._2))
     .foldLeft(Empty.asInstanceOf[Criterion])((c1, c2) => c1 and c2)
  } 
}

class Empty extends Criterion{
  def toSql = ""
  def toPrepareSql = ""
  def getParams = Nil
}

object Empty extends Empty

class OrCriterion(c1: Criterion, c2: Criterion) extends Criterion{
  def toSql = "( " + c1.toSql + " OR " + c2.toSql + " )"
  def toPrepareSql = "( " + c1.toPrepareSql + " OR " + c2.toPrepareSql + " )"
  def getParams = c1.getParams ++ c2.getParams
}

class AndCriterion(c1: Criterion, c2: Criterion) extends Criterion{
  private[this] def convert(c1: Criterion, c2: Criterion)(fn: Criterion => String): String =
    (c1, c2) match{
    case (Empty, Empty) => ""
    case (Empty, c2) => fn(c2)
    case (c1, Empty) => fn(c1)
    case _ => "( " + fn(c1) + " AND " + fn(c2) + " )" 
  }
  
  def toSql = convert(c1,c2)(_.toSql)
  def toPrepareSql = convert(c1,c2)(_.toPrepareSql)
  def getParams = c1.getParams ++ c2.getParams
}

abstract class Comparator(val symbol: String){
  def toSql(value: Any): String
  def toPrepareSql(firstTerm: FirstTerm): String
}

class EqualCmp extends Comparator("="){
  def toSql(value: Any) = value match{
    case i: Int => symbol + " " + i
    case _ => symbol + " '" + value.toString() + "'"  
  } 
  def toPrepareSql(firstTerm: FirstTerm) = symbol + " " + firstTerm.toPrepareSql 
}

class InCmp extends Comparator("IN"){
  def toSql(value: Any) = symbol + " " + (value match {
    case s: Seq[_] => "(" + s.map("\""+_.toString()+"\"").mkString(",") + ")"
    case other => other.toString() 
  })
  
  def toPrepareSql(firstTerm: FirstTerm) = symbol + " (" + firstTerm.toPrepareSql + ")"
}