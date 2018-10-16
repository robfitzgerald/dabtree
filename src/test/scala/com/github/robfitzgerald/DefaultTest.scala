package com.github.robfitzgerald

import org.scalatest._

abstract class DefaultTest extends WordSpec with Matchers with PrivateMethodTester with BeforeAndAfter {}

