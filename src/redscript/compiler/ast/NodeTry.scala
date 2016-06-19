package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeTry(val body: NodeStatement, val excepts: List[NodeExcept], val finalizer: Option[NodeStatement]) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}

class NodeExcept(val name: Option[NodeName], val excepts: List[List[NodeName]], val body: NodeStatement) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
