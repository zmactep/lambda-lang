package me.yakovlev.lambda

import me.yakovlev.lambda.interactive.{Result, Environment, Statement}
import me.yakovlev.lambda.parsers.InteractiveParser

/**
  * Created by pavel on 13.03.16.
  */
object REPL {
  def main(args : Array[String]) : Unit = {
    repl()
  }

  def read(line : String) : Statement = {
    InteractiveParser(line)
  }

  def eval(statement : Statement, environment : Environment) : Environment = {
    environment.run(statement)
  }

  def print(result : Result) : Unit = {
    println(result.show)
  }

  def repl() : Unit = {
    Iterator.from(1)
      .map {
        case _ =>
          Option(io.StdIn.readLine("> "))
      }
      .takeWhile(_.isDefined)
      .flatten
      .foldLeft(Environment.create) {
        case (env, line) =>
          if (!line.isEmpty) {
            val nenv = eval(read(line), env)
            print(nenv.lastResult)
            nenv
          }
          else {
            env
          }
      }
    println("\nBye!")
  }
}
