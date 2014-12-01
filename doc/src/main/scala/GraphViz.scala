package uk.org.openeyes.jsonschema.doc

import java.io.Writer
import text.Document
import text.Document._

object GraphViz {
  trait Renderable {
    def render: Option[Document]
  }

  case class Graph(params: Params, nodeSets: RenderableSeq[NodeSet], edgeSets: RenderableSeq[EdgeSet]) extends Renderable {
    def format(w: Writer) = render.map(_.format(0, w))
    def render = Container("digraph G", "{", combine(params, nodeSets, edgeSets), "}").render.map(_ :: break)
  }
  object Graph {
    def apply(params: (String, String)*)(nodeSets: NodeSet*)(edgeSets: EdgeSet*): Graph = {
      apply(Params(params), RenderableSeq(nodeSets), RenderableSeq(edgeSets))
    }
  }

  case class NodeSet(params: Params, nodes: RenderableSeq[Node]) extends Renderable {
    def render = combine(Container("node", "[", params.render, "]"), nodes)
  }
  object NodeSet {
    def apply(params: (String, String)*)(nodes: Seq[Node]): NodeSet = apply(Params(params), RenderableSeq(nodes))
  }

  case class Node(name: String, params: Params = Params()) extends Renderable {
    def render = Container(quote(name), "[", params.render, "]").render
  }
  object Node {
    def apply(name: String, params: (String, String)*): Node = apply(name, Params(params))
  }

  case class EdgeSet(params: Params, edges: RenderableSeq[Edge]) extends Renderable {
    def render = combine(Container("edge", "[", params.render, "]"), edges)
  }
  object EdgeSet {
    def apply(params: (String, String)*)(edges: Seq[Edge]): EdgeSet = apply(Params(params), RenderableSeq(edges))
  }

  case class Edge(ends: (String, String)) extends Renderable {
    def render = Some(text(quote(ends._1) + " -> " + quote(ends._2)))
  }

  type Params = RenderableSeq[Param]
  object Params {
    def apply(params: Seq[(String, String)] = Seq()) = RenderableSeq(params.map(Param))
  }

  case class Param(param: (String, String)) extends Renderable {
    def render = Some(text(quote(param._1) + " = " + quote(param._2)))
  }

  case class RenderableSeq[T <: Renderable](items: Seq[T] = Seq()) extends Renderable {
    def render = combine(items: _*)
  }

  case class Container(name: String, startDelim: String, content: Option[Document], endDelim: String) extends Renderable {
    def render = content map (c => text(name + " " + startDelim) :: nest(4, break :: c) :/: text(endDelim))
  }

  def combine(rs: Renderable*): Option[Document] = rs.flatMap(_.render).reduceLeftOption(_ :/: _)

  def quote(str: String) = '"' + str + '"'
}
