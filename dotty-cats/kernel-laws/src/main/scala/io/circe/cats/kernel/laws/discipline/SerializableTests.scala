package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.laws.SerializableLaws
import org.typelevel.discipline.Laws

object SerializableTests extends Laws {
  def serializable[A](a: A): RuleSet =
    new DefaultRuleSet(name = "serializable",
                       parent = None,
                       "can serialize and deserialize" -> SerializableLaws.serializable(a))
}
