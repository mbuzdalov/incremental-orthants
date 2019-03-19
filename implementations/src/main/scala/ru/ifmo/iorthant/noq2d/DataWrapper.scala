package ru.ifmo.iorthant.noq2d

import ru.ifmo.iorthant.util.Specialization

class DataWrapper[@specialized(Specialization.defaultSet) T](val point: Array[Double], val value: T)
