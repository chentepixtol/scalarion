package org.scalarion.test

import org.scalatest.FunSuite
import org.scalarion.Conversions._
import org.scalarion.Criterion

class FirstTest extends FunSuite {
   
  test("equal"){
    expect("`username` = 'chentepixtol'"){'username equal "chentepixtol" toSql }
    expect("`username` = {username}"){'username equal "chentepixtol" toPrepareSql }
    expect(List('username -> "chentepixtol")){'username equal "chentepixtol" getParams }
  }
  
  test("or"){
    expect("( `username` = 'chentepixtol' OR `username` = 'vicentemmor' )"){
      ('username equal "chentepixtol") or ('username equal "vicentemmor") toSql 
    }
    
    expect("( `username` = {username} OR `username` = {username} )"){
      ('username equal "chentepixtol") or ('username equal "vicentemmor") toPrepareSql 
    }
    
    expect(List('username -> "chentepixtol", 'username -> "vicentemmor")){
      ('username equal "chentepixtol") or ('username equal "vicentemmor") getParams 
    }
  }
  
  test("and"){
    
    expect("( `username` = 'chentepixtol' AND `username` = 'vicentemmor' )"){
      ('username equal "chentepixtol") and ('username equal "vicentemmor") toSql 
    }
    
    expect("( `username` = {username} AND `username` = {username} )"){
      ('username equal "chentepixtol") and ('username equal "vicentemmor") toPrepareSql 
    }
    
    expect(List('username -> "chentepixtol", 'username -> "vicentemmor")){
      ('username equal "chentepixtol") and ('username equal "vicentemmor") getParams 
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
  
  test(" in "){
    expect("""`user_id` IN ("1","2","3")"""){
      'user_id.in(Seq(1,2,3)) toSql
    }
    
    expect(List('user_id -> Seq(1,2,3))){
      'user_id.in(Seq(1,2,3)) getParams
    }
    
    expect("""`lettres` IN ("a","b","c","d","e")"""){
      'lettres.in(Seq("a","b","c","d","e")) toSql
    }
    
    expect(List('lettres -> Seq("a","b","c","d","e"))){
      'lettres.in(Seq("a","b","c","d","e")) getParams
    }
  }
  
  test("mutator"){
    expect("SUM(`id_user`) = 100"){
      'id_user.mutator("SUM").equal(100) toSql
    }
    
    expect("SUM(`id_user`) = 100"){
      'id_user.sum.equal(100) toSql
    }
    
    expect(List('id_user -> 100)){
      'id_user.sum.equal(100) getParams
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