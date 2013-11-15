package proteins

object DNADirection extends Enumeration{
    type DNADirection = Value
    val FIVE_TO_THREE,THREE_TO_FIVE = Value

    def oposite(direction:DNADirection):DNADirection = direction match {
        case FIVE_TO_THREE => THREE_TO_FIVE
        case THREE_TO_FIVE => FIVE_TO_THREE
    }

}
