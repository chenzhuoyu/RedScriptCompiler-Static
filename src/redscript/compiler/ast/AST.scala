package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

import scala.util.parsing.input.Positional

abstract class AST extends Positional
{
    var vtype: Class[_] = null
    def assemble(generator: GeneratorAdapter)
}