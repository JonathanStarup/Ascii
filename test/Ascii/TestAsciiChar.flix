mod Ascii.Test.TestAsciiChar {
    use Ascii.AsciiChar

    @Test
    pub def testLowerCase(): Unit \ Assert = {
        use AsciiChar.fromCharUnsafe;
        use AsciiChar.toLowerCase;
        def test(c1, c2) =
            Assert.assertEq(expected = fromCharUnsafe(c2), toLowerCase(fromCharUnsafe(c1)));
        test('A', 'a');
        test('D', 'd');
        test('Z', 'z');
        test('a', 'a');
        test('1', '1');
        test(' ', ' ');
        test('\u0001', '\u0001')
    }

    @Test
    pub def testUpperCase(): Unit \ Assert = {
        use AsciiChar.fromCharUnsafe;
        use AsciiChar.toUpperCase;
        def test(c1, c2) =
            Assert.assertEq(expected = fromCharUnsafe(c2), toUpperCase(fromCharUnsafe(c1)));
        test('a', 'a');
        test('d', 'D');
        test('z', 'Z');
        test('A', 'A');
        test('1', '1');
        test(' ', ' ');
        test('\u0001', '\u0001')
    }

}
