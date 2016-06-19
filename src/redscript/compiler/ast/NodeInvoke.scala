package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeActualArg(val expr: NodeExpr, val expand: Boolean) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}

class NodeInvokeAttr(val attr: NodeName, val invoke: NodeInvokeStack) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}

class NodeInvokeStack(val args: List[NodeActualArg]) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
