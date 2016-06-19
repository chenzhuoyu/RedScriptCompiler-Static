package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeStatement(val node: AST) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit =
    {
        node.assemble(generator)
        node match
        {
            case _: NodeExpr  => generator.pop()
            case _: NodeValue => generator.pop()
            case _ =>
        }
    }
}
