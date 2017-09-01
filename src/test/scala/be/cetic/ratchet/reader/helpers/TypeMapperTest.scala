package be.cetic.ratchet.reader.helpers

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{WordSpec, FlatSpec, Matchers, FunSuite}
import be.cetic.ratchet.reader.ast._

/**
 * Test suites for TypeMapper
 *
 * TypeMapper(signed:Boolean,extern:Boolean,primitive:List[String],declType:TypeSpecifier)
 *
 * @author : dimitri durieux <dimitri.durieux@cetic.be>
 */
@RunWith(classOf[JUnitRunner])
class TypeMapperTest extends FlatSpec with Matchers{

  "getSize Method" should "return only short and long specifier" in {
    val mapper = TypeMapper(false,List("short","long","int"),None,None)
    assert(mapper.size==(List("short","long")))
  }

  it should "return empty list when no one size specifier is set" in {
    val mapper = TypeMapper(false,List("int"),None,None)
    assert(mapper.size == Nil)
  }

  "generateType" should "return 32 bits integer when specifier is unsigned int" in{
    val mapper = TypeMapper(false,List("int"),None,None)
    assert(mapper.generateType == IntType(32,false))
  }

  it should "return 16 bits integer when specifier is unsigned short int" in{
    val mapper = TypeMapper(false,List("short","int"),None,None)
    assert(mapper.generateType == IntType(16,false))
  }

  it should "return typedef reference when the type decl references to typedef " in{
    val mapper = TypeMapper(false,List(),
      Some(TypeDefReference(TypeDecl("UINT32",IntType()))),None)
    mapper.generateType should equal (TypeDefReference(TypeDecl("UINT32",IntType())))
  }

  it should "return 64 bits integer when specifier is unsigned long long" in{
    val mapper = TypeMapper(false,List("long","long"),None,None)
    assert(mapper.generateType == IntType(64,false))
  }

  it should "return int f() when specifier is int and function type is ()" in {
    val mapper = TypeMapper(true,List("int"),None,
      Some(SuffixedDeclarator(Identifier("f"),List(FunctionDeclaration(Nil.asInstanceOf[List[VarDecl]])),None))
      )
    mapper.generateType should equal (FunctionType(IntType(32,true),Nil))
  }

  it should "return void f(int i) when specifier is void and function type is (int i)" in {
    val mapper = TypeMapper(false,List("void"),None,
      Some(SuffixedDeclarator(Identifier("f"),List(FunctionDeclaration(List(VarDecl("i",false,false,IntType(32,false))))),None))
    )
    mapper.generateType should equal (FunctionType(Void(),List(VarDecl("i",false,false,IntType(32,false)))))
  }

  it should "return int f(int i) when specifier is signed int and function type is (int i)" in {
    val mapper = TypeMapper(true,List("int"),None,
      Some(SuffixedDeclarator(Identifier("f"),List(FunctionDeclaration(List(VarDecl("i",false,false,IntType(32,false))))),None))
    )
    mapper.generateType should equal (FunctionType(IntType(32,true),List(VarDecl("i",false,false,IntType(32,false)))))
  }

  it should "return int* when specifier is int and declator a simple pointer" in{
    val mapper = TypeMapper(true,List("int"),None,
      Some(SuffixedDeclarator(Anonymous(),Nil,Some(PointerTemplate(1)))))
    mapper.generateType should equal (PointerType(IntType(32,true)))
  }

  it should "return unsigned int* when specifier is unsigned int and declator a simple pointer" in{
    val mapper = TypeMapper(false,List("int"),None,
      Some(SuffixedDeclarator(Anonymous(),Nil,Some(PointerTemplate(1)))))
    mapper.generateType should equal (PointerType(IntType(32,false)))
  }

  it should "return unsigned int** when specifier is unsigned int and declator a double pointer" in{
    val mapper = TypeMapper(false,List("int"),None,
      Some(SuffixedDeclarator(Anonymous(),Nil,Some(PointerTemplate(2)))))
    mapper.generateType should be (PointerType(PointerType(IntType(32,false))))
  }

  it should "return unsigned int*** when specifier is unsigned int and declator a triple pointer" in{
    val mapper = TypeMapper(false,List("int"),None,
      Some(SuffixedDeclarator(Anonymous(),Nil,Some(PointerTemplate(3)))))
    mapper.generateType should be (PointerType(PointerType(PointerType(IntType(32,false)))))
  }

  it should "return void (f*)(int i) when specifier is void and function type is (int i)" in{
    val mapper = TypeMapper(false,List("void"),None,
        Some(SuffixedDeclarator(Identifier("f"),List(FunctionDeclaration(List(VarDecl("i",false,false,IntType(32,false))))),
          Some(PointerTemplate(1)))
        )
      )
    mapper.generateType should equal (PointerType(FunctionType(Void(),List(VarDecl("i",false,false,IntType(32,false))))))
  }

  it should "return void (f*)(int) when specifier is void and function type is (int)" in{
    val mapper = TypeMapper(false,List("void"),None,
      Some(SuffixedDeclarator(Identifier("f"),List(FunctionDeclaration(List(VarDecl("",false,false,IntType(32,false))))),
        Some(PointerTemplate(1)))
      )
    )
    mapper.generateType should equal (PointerType(FunctionType(Void(),List(VarDecl("",false,false,IntType(32,false))))))
  }
}
