mod Ascii {

    /// A byte-represented ascii character.
    ///
    /// Always in the range [0, 127].
    ///
    pub enum AsciiChar(Int8)

    instance Eq[AsciiChar] {
        pub def eq(ac1: AsciiChar, ac2: AsciiChar): Bool =
            AsciiChar.eq(ac1, ac2)
    }

    instance ToString[AsciiChar] {
        pub def toString(ac: AsciiChar): String =
            AsciiChar.toString(ac)
    }

}

mod Ascii.AsciiChar {
    import java.lang.Character

    pub def fromInt8Opt(i: Int8): Option[AsciiChar] = {
        i |> fromInt8Res |> Result.toOption
    }

    pub def fromInt8Res(i: Int8): Result[String, AsciiChar] = {
        if (0i8 <= i) Result.Ok(AsciiChar(i))
        else Result.Err("${i} is outside the range of ASCII")
    }

    pub def fromInt8(i: Int8): AsciiChar \ Abort = {
        match fromInt8Res(i) {
            case Result.Ok(ac) => ac
            case Result.Err(msg) => Abort.abort(msg)
        }
    }

    pub def fromInt8Unsafe(i: Int8): AsciiChar = {
        match fromInt8Res(i) {
            case Result.Ok(ac) => ac
            case Result.Err(msg) => bug!(msg)
        }
    }

    pub def fromCharOpt(c: Char): Option[AsciiChar] = {
        c |> fromCharRes |> Result.toOption
    }

    pub def fromCharRes(c: Char): Result[String, AsciiChar] = {
        let codePoint = Char.toBmpCodePoint(c);
        match Int32.tryToInt8(codePoint) {
            case Some(i) if 0i8 <= i => Result.Ok(AsciiChar(i))
            case _ => Result.Err("'${c}' has code point ${codePoint}, outside the range of ASCII")
        }
    }

    pub def fromChar(c: Char): AsciiChar \ Abort = {
        match fromCharRes(c) {
            case Result.Ok(ac) => ac
            case Result.Err(msg) => Abort.abort(msg)
        }
    }

    pub def fromCharUnsafe(c: Char): AsciiChar = {
        match fromCharRes(c) {
            case Result.Ok(ac) => ac
            case Result.Err(msg) => bug!(msg)
        }
    }

    pub def toInt8(ac: AsciiChar): Int8 = {
        let AsciiChar(i) = ac;
        i
    }

    pub def toChar(ac: AsciiChar): Char = unsafely IO run {
        Character.toChars(ac |> toInt8 |> Int8.toInt32) |> Array.get(0)
    }

    pub def toString(ac: AsciiChar): String = {
        ac |> toChar |> Char.toString
    }

    pub def toDisplayString(ac: AsciiChar): String = {
        let i = toInt8(ac);
        if (33i8 <= i and i <= 126i8) toString(ac)
        else {
            match i {
                case   0i8 => "0x00"
                case   1i8 => "0x01"
                case   2i8 => "0x02"
                case   3i8 => "0x03"
                case   4i8 => "0x04"
                case   5i8 => "0x05"
                case   6i8 => "0x06"
                case   7i8 => "0x07"
                case   8i8 => "0x08"
                case   9i8 => "\\t"
                case  10i8 => "\\n"
                case  11i8 => "0x0B"
                case  12i8 => "0x0C"
                case  13i8 => "\\r"
                case  14i8 => "0x0E"
                case  15i8 => "0x0F"
                case  16i8 => "0x10"
                case  17i8 => "0x11"
                case  18i8 => "0x12"
                case  19i8 => "0x13"
                case  20i8 => "0x14"
                case  21i8 => "0x15"
                case  22i8 => "0x16"
                case  23i8 => "0x17"
                case  24i8 => "0x18"
                case  25i8 => "0x19"
                case  26i8 => "0x1A"
                case  27i8 => "0x1B"
                case  28i8 => "0x1C"
                case  29i8 => "0x1D"
                case  30i8 => "0x1E"
                case  31i8 => "0x1F"
                case  32i8 => "0x20"
                case 127i8 => "0x7F"
                case _ => bug!("Ascii char with value ${toInt8(ac)} found.")
            }
        }
    }

    /// Note that in https://www.ascii-code.com/
    ///  - \t (9 / 0x09) is NOT a printable character
    ///  - \n (10 / 0x0A) is NOT a printable character
    ///  - \r (13 / 0x0D) is NOT a printable character
    ///  - Delete (128 / 0x7F) IS a printable character
    pub def isPrintableOfficial(ac: AsciiChar): Bool = {
        32i8 <= toInt8(ac)
    }

    /// Returns `true` if `ac` is practically printable.
    pub def isPrintable(ac: AsciiChar): Bool = {
        let i = toInt8(ac);
        (32i8 <= i and i <= 126i8) or i == 9i8 or i == 10i8 or i == 13i8
    }

    /// Consistent with `Char.isWhitespace`.
    pub def isWhitespaceOfficial(ac: AsciiChar): Bool = {
        let i = toInt8(ac);
        (9i8 <= i and i <= 13i8) or (28i8 <= i and i <= 32i8)
    }

    /// Returns `true` for `\r`, `\t`, `\n`, and ` `.
    pub def isWhitespace(ac: AsciiChar): Bool = {
        match toInt8(ac) {
            case  9i8 => true
            case 10i8 => true
            case 13i8 => true
            case 32i8 => true
            case    _ => false
        }
    }

    /// Consistent with `Char.isLetter` (a-z and A-Z).
    pub def isLetter(ac: AsciiChar): Bool = {
        let i = toInt8(ac);
        isUpperCaseInt8(i) or isLowerCaseInt8(i)
    }

    /// Consistent with `Char.isUpperCase` (A-Z).
    pub def isUpperCase(ac: AsciiChar): Bool = {
        ac |> toInt8 |> isUpperCaseInt8
    }

    def isUpperCaseInt8(i: Int8): Bool =
        65i8 <= i and i <= 90i8

    /// Consistent with `Char.isLowerCase` (a-z).
    pub def isLowerCase(ac: AsciiChar): Bool = {
        ac |> toInt8 |> isLowerCaseInt8
    }

    def isLowerCaseInt8(i: Int8): Bool =
        97i8 <= i and i <= 122i8

    /// Consistent with `Char.isDigit` (0-9).
    pub def isDigit(ac: AsciiChar): Bool = {
        ac |> toInt8 |> isDigitInt8
    }

    def isDigitInt8(i: Int8): Bool =
        48i8 <= i and i <= 57i8

    /// Consistent with `Char.isHexDigit` (0-9, a-f, A-F).
    pub def isHexDigit(ac: AsciiChar): Bool = {
        let i = toInt8(ac);
        isDigitInt8(i) or
        (65i8 <= i and i <= 70i8) or
        (97i8 <= i and i <= 102i8)
    }

    /// Consistent with `Char.isOctDigit` (0-7).
    pub def isOctDigit(ac: AsciiChar): Bool = {
        let i = toInt8(ac);
        48i8 <= i and i <= 55i8
    }

    /// (a-z, A-Z, or 0-9)
    pub def isLetterOrDigit(ac: AsciiChar): Bool = {
        let i = toInt8(ac);
        isDigitInt8(i) or
        isLowerCaseInt8(i) or
        isUpperCaseInt8(i)
    }

    /// Does nothing if `ac` is not an upper-case letter.
    pub def toLowerCase(ac: AsciiChar): AsciiChar = {
        let i = toInt8(ac);
        if (isUpperCaseInt8(i)) {
            AsciiChar(Int8.bitwiseXor(i, 32i8)) // 32 is 0010_0000
        } else {
            ac
        }
    }

    /// Does nothing if `ac` is not a lower-case letter.
    pub def toUpperCase(ac: AsciiChar): AsciiChar = {
        let i = toInt8(ac);
        if (isLowerCaseInt8(i)) {
            AsciiChar(Int8.bitwiseXor(i, 32i8)) // 32 is 0010_0000
        } else {
            ac
        }
    }

    pub def eq(ac1: AsciiChar, ac2: AsciiChar): Bool = {
        let AsciiChar(i1) = ac1;
        let AsciiChar(i2) = ac2;
        i1 == i2
    }

    pub def description(ac: AsciiChar): String = {
        // See https://www.ascii-code.com/
        match toInt8(ac) {
            case   0i8 => "Null character"
            case   1i8 => "Start of Heading"
            case   2i8 => "Start of Text"
            case   3i8 => "End of Text"
            case   4i8 => "End of Transmission"
            case   5i8 => "Enquiry"
            case   6i8 => "Acknowledge"
            case   7i8 => "Bell, Alert"
            case   8i8 => "Backspace"
            case   9i8 => "Horizontal Tab"
            case  10i8 => "Line Feed"
            case  11i8 => "Vertical Tabulation"
            case  12i8 => "Form Feed"
            case  13i8 => "Carriage Return"
            case  14i8 => "Shift Out"
            case  15i8 => "Shift In"
            case  16i8 => "Data Link Escape"
            case  17i8 => "Device Control One (XON)"
            case  18i8 => "Device Control Two"
            case  19i8 => "Device Control Three (XOFF)"
            case  20i8 => "Device Control Four"
            case  21i8 => "Negative Acknowledge"
            case  22i8 => "Synchronous Idle"
            case  23i8 => "End of Transmission Block"
            case  24i8 => "Cancel"
            case  25i8 => "End of medium"
            case  26i8 => "Substitute"
            case  27i8 => "Escape"
            case  28i8 => "File Separator"
            case  29i8 => "Group Separator"
            case  30i8 => "Record Separator"
            case  31i8 => "Unit Separator"
            case  32i8 => "Space"
            case  33i8 => "Exclamation mark"
            case  34i8 => "Double quotes (or speech marks)"
            case  35i8 => "Number sign"
            case  36i8 => "Dollar"
            case  37i8 => "Per cent sign"
            case  38i8 => "Ampersand"
            case  39i8 => "Single quote"
            case  40i8 => "Open parenthesis (or open bracket)"
            case  41i8 => "Close parenthesis (or close bracket)"
            case  42i8 => "Asterisk"
            case  43i8 => "Plus"
            case  44i8 => "Comma"
            case  45i8 => "Hyphen-minus"
            case  46i8 => "Period, dot or full stop"
            case  47i8 => "Slash or divide"
            case  48i8 => "Zero"
            case  49i8 => "One"
            case  50i8 => "Two"
            case  51i8 => "Three"
            case  52i8 => "Four"
            case  53i8 => "Five"
            case  54i8 => "Six"
            case  55i8 => "Seven"
            case  56i8 => "Eight"
            case  57i8 => "Nine"
            case  58i8 => "Colon"
            case  59i8 => "Semicolon"
            case  60i8 => "Less than (or open angled bracket)"
            case  61i8 => "Equals"
            case  62i8 => "Greater than (or close angled bracket)"
            case  63i8 => "Question mark"
            case  64i8 => "At sign"
            case  65i8 => "Uppercase A"
            case  66i8 => "Uppercase B"
            case  67i8 => "Uppercase C"
            case  68i8 => "Uppercase D"
            case  69i8 => "Uppercase E"
            case  70i8 => "Uppercase F"
            case  71i8 => "Uppercase G"
            case  72i8 => "Uppercase H"
            case  73i8 => "Uppercase I"
            case  74i8 => "Uppercase J"
            case  75i8 => "Uppercase K"
            case  76i8 => "Uppercase L"
            case  77i8 => "Uppercase M"
            case  78i8 => "Uppercase N"
            case  79i8 => "Uppercase O"
            case  80i8 => "Uppercase P"
            case  81i8 => "Uppercase Q"
            case  82i8 => "Uppercase R"
            case  83i8 => "Uppercase S"
            case  84i8 => "Uppercase T"
            case  85i8 => "Uppercase U"
            case  86i8 => "Uppercase V"
            case  87i8 => "Uppercase W"
            case  88i8 => "Uppercase X"
            case  89i8 => "Uppercase Y"
            case  90i8 => "Uppercase Z"
            case  91i8 => "Opening bracket"
            case  92i8 => "Backslash"
            case  93i8 => "Closing bracket"
            case  94i8 => "Caret - circumflex"
            case  95i8 => "Underscore"
            case  96i8 => "Grave accent"
            case  97i8 => "Lowercase a"
            case  98i8 => "Lowercase b"
            case  99i8 => "Lowercase c"
            case 100i8 => "Lowercase d"
            case 101i8 => "Lowercase e"
            case 102i8 => "Lowercase f"
            case 103i8 => "Lowercase g"
            case 104i8 => "Lowercase h"
            case 105i8 => "Lowercase i"
            case 106i8 => "Lowercase j"
            case 107i8 => "Lowercase k"
            case 108i8 => "Lowercase l"
            case 109i8 => "Lowercase m"
            case 110i8 => "Lowercase n"
            case 111i8 => "Lowercase o"
            case 112i8 => "Lowercase p"
            case 113i8 => "Lowercase q"
            case 114i8 => "Lowercase r"
            case 115i8 => "Lowercase s"
            case 116i8 => "Lowercase t"
            case 117i8 => "Lowercase u"
            case 118i8 => "Lowercase v"
            case 119i8 => "Lowercase w"
            case 120i8 => "Lowercase x"
            case 121i8 => "Lowercase y"
            case 122i8 => "Lowercase z"
            case 123i8 => "Opening brace"
            case 124i8 => "Vertical bar"
            case 125i8 => "Closing brace"
            case 126i8 => "Equivalency sign - tilde"
            case 127i8 => "Delete"
            case _ => bug!("Ascii char with value ${toInt8(ac)} found.")
        }
    }

}
