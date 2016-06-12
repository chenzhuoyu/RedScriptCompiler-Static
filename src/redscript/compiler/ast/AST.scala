package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

import scala.util.parsing.input.Positional

abstract class AST extends Positional
{
    var vtype: Class[_] = classOf[Any]
    def assemble(method: MethodVisitor)
}