package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeField(val name: NodeName, val expr: NodeExpr) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}

class NodeClass(
    val name        : NodeName,
    val parent      : List[NodeName],
    val interfaces  : List[List[NodeName]],
    val fields      : List[NodeField],
    val methods     : List[NodeFunctionDef],
    val classes     : List[NodeClass]
) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
