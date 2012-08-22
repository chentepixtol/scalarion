package org.scalarion.test

import org.scalatest.FunSuite
import org.scalarion.Conversions._
import org.scalarion.Criterion

class FirstTest extends FunSuite {
   
  test("equal"){
    val criterion = 'username equal "chentepixtol"
    expect("`username` = 'chentepixtol'"){ criterion.toSql }
    expect("`username` = {username}"){ criterion.toPrepareSql }
    expect(List('username -> "chentepixtol")){ criterion.getParams }
  }
  
  test("or"){
    val criterion = ('username equal "chentepixtol") or ('username equal "vicentemmor")
    expect("( `username` = 'chentepixtol' OR `username` = 'vicentemmor' )"){
      criterion.toSql 
    }
    expect("( `username` = {username} OR `username` = {username} )"){
      criterion.toPrepareSql 
    }
    expect(List('username -> "chentepixtol", 'username -> "vicentemmor")){
      criterion.getParams 
    }
  }
  
  test("and"){
    val criterion = ('username equal "chentepixtol") and ('username equal "vicentemmor")
    expect("( `username` = 'chentepixtol' AND `username` = 'vicentemmor' )"){
      criterion.toSql 
    }
    expect("( `username` = {username} AND `username` = {username} )"){
      criterion.toPrepareSql 
    }
    expect(List('username -> "chentepixtol", 'username -> "vicentemmor")){
      criterion.getParams 
    }
  }
  
  test("complex"){
    val complex = ( 'username.equal("chentepixtol") and 'username.equal("vicentemmor") ) or 'email.equal("yahoo")
    expect("( ( `username` = 'chentepixtol' AND `username` = 'vicentemmor' ) OR `email` = 'yahoo' )"){  
      complex.toSql
    }
    expect("( ( `username` = {username} AND `username` = {username} ) OR `email` = {email} )"){
      complex.toPrepareSql
    }
    expect(List('username -> "chentepixtol", 'username -> "vicentemmor", 'email -> "yahoo")){
      complex.getParams
    }
  }
  
  test(" in with Int "){
    val criterion = 'user_id.in(Seq(1,2,3))
    expect("""`user_id` IN ("1","2","3")"""){
      criterion.toSql
    }
    expect(List('user_id -> Seq(1,2,3))){
      criterion.getParams
    }
  }

  test("in with string"){
    val criterion = 'lettres.in(Seq("a","b","c","d","e"))
    expect("""`lettres` IN ("a","b","c","d","e")"""){
      criterion.toSql
    }
    expect(List('lettres -> Seq("a","b","c","d","e"))){
      criterion.getParams
    }
  }
  
  test("mutators"){
    val criterion = 'id_user.sum.equal(100)
    expect("SUM(`id_user`) = 100"){
      criterion.toSql
    }
    expect(List('id_user -> 100)){
      criterion.getParams
    } 
  }
  
  test("filters"){
    val filters = Map(
      "Invalid" -> None,
      "username" -> "chentepixtol",
      "age" -> 26,
      "mail" -> "yahoo",
      "Other" -> None
    )
    val query = Criterion(filters)
    expect(List('username -> "chentepixtol", 'age -> 26, 'mail -> "yahoo")){
      query.getParams
    }
    expect("( ( `username` = 'chentepixtol' AND `age` = 26 ) AND `mail` = 'yahoo' )"){
      query.toSql
    }
  }
  
}