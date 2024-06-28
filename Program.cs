using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Text.RegularExpressions;

namespace SubleqxAsm
{
    class OutputBitStream
    {
        int bitIndex = 0;
        byte byteValue = 0;
        List<byte> data;

        public OutputBitStream()
        {
            data = new List<byte>();
        }

        public void WriteValue(long value, int size)
        {
            for (int i = 0; i < size; i++)
            {
                if ((value & 1 << i) != 0)
                    byteValue |= (byte)(1 << bitIndex);
                bitIndex++;
                if (bitIndex > 7)
                {
                    data.Add(byteValue);
                    byteValue = 0;
                    bitIndex = 0;
                }
            }
        }

        public byte[] GetData()
        {
            if (bitIndex != 0)
                data.Add(byteValue);
            return data.ToArray();
        }
    }

    class Token
    {
        public bool IsOperator;
        public string Value;
        public BigInteger? IntValue;
    }

    class Program
    {
        OutputBitStream obs;

        int GetSizeLog(int value)
        {
            int result = 0;
            int thr = 1;
            for (int i = 1; i < 32; i++)
            {
                if (value >= thr)
                    result = i;
                thr <<= 1;
            }
            return result;
        }

        void WriteData(long value, int size)
        {
            CheckInteger(value, size);
            obs.WriteValue(value, size);
            Console.Write($"{value} ");
        }

        void WriteGammaCode(int value)
        {
            if (value <= 0)
                throw new Exception();
            int slm1 = GetSizeLog(value) - 1;
            obs.WriteValue(0, slm1);
            obs.WriteValue(1, 1);
            obs.WriteValue(value - (1 << slm1), slm1);
            Console.Write($"{value} ");
        }

        uint GetGammaCodeLength(int value)
        {
            if (value <= 0)
                throw new Exception();
            int sl = GetSizeLog(value);
            return (uint)(2 * sl - 1);
        }

        List<Token> TokenizeExpression(string expression)
        {
            var tokens = new List<Token>();
            string value = "";
            for (int i = 0; i < expression.Length; i++)
            {
                char c = expression[i];
                if (c == ' ' || c == '\t')
                    continue;
                if (c == '(' || c == ')' || c == '*' || c == '/' || c == '+' || c == '-')
                {
                    if (value != "")
                    {
                        tokens.Add(new Token { IsOperator = false, Value = value });
                        value = "";
                    }
                    if (c == '-' && (tokens.Count == 0 ||
                        (tokens.Last().IsOperator && tokens.Last().Value != ")")))
                    {
                        c = 'u';
                    }
                    tokens.Add(new Token { IsOperator = true, Value = c.ToString() });
                }
                else
                    value += c;
            }
            if (value != "")
                tokens.Add(new Token { IsOperator = false, Value = value });
            return tokens;
        }

        int GetPrecedence(Token op)
        {
            if (!op.IsOperator)
                throw new Exception();
            string v = op.Value;
            if (v == "+" || v == "-")
                return 0;
            else if (v == "*" || v == "/")
                return 1;
            else if (v == "u")
                return 2;
            else
                throw new Exception();
        }

        List<Token> ShuntingYard(List<Token> tokens)
        {
            var result = new List<Token>();
            var stack = new Stack<Token>();

            foreach (var token in tokens)
            {
                if (!token.IsOperator)
                    result.Add(token);
                else if (token.Value == "(")
                    stack.Push(token);
                else if (token.Value == ")")
                {
                    while (stack.Peek().Value != "(")
                        result.Add(stack.Pop());
                    if (stack.Pop().Value != "(")
                        throw new Exception();
                }
                else
                {
                    int p1 = GetPrecedence(token);
                    for (;;)
                    {
                        if (stack.Count == 0)
                            break;
                        Token last = stack.Peek();
                        if (last.Value == "(")
                            break;
                        int p2 = GetPrecedence(last);
                        if (p2 >= p1)
                            result.Add(stack.Pop());
                        else
                            break;
                    }
                    stack.Push(token);
                }
            }

            while (stack.Count() != 0)
                result.Add(stack.Pop());

            return result;
        }

        public static BigInteger ParseInteger(string s)
        {
            string sn = s;
            bool hex = false;
            if (sn.StartsWith("0x"))
            {
                hex = true;
                sn = "0" + sn.Substring(2);
            }
            return BigInteger.Parse(sn, hex ? NumberStyles.HexNumber : 0);
        }

        long Evaluate(List<Token> tokens)
        {
            var stack = new Stack<Token>();
            foreach (var token in tokens)
            {
                if (!token.IsOperator)
                {
                    if (token.IntValue == null)
                        token.IntValue = ParseInteger(token.Value);
                    stack.Push(token);
                }
                else
                {
                    if (token.Value != "u")
                    {
                        BigInteger right = (BigInteger)stack.Pop().IntValue;
                        BigInteger left = (BigInteger)stack.Pop().IntValue;
                        BigInteger result;
                        if (token.Value == "+")
                            result = left + right;
                        else if (token.Value == "-")
                            result = left - right;
                        else if (token.Value == "*")
                            result = left * right;
                        else if (token.Value == "/")
                            result = left / right;
                        else
                            throw new Exception();
                        stack.Push(new Token { IsOperator = false, IntValue = result });
                    }
                    else
                        stack.Push(new Token { IsOperator = false, IntValue = -stack.Pop().IntValue });
                }
            }
            if (stack.Count != 1)
                throw new Exception();
            return (long)stack.Pop().IntValue;
        }

        static void CheckInteger(BigInteger integer, int width)
        {
            BigInteger limit = BigInteger.One;
            limit <<= width;
            if (integer < 0)
                integer = -integer;
            if (integer >= limit)
                throw new Exception();
        }

        void Assemble(string inputFile, string outputFile, ulong baseAddr)
        {
            obs = new OutputBitStream();

            var lines = File.ReadAllLines(inputFile);

            var labels = new Dictionary<string, ulong>();
            for (int pass = 1; pass <= 2; pass++)
            {
                ulong ip = baseAddr;
                foreach (var line in lines)
                {
                    int i = 0;
                    int aw = 0;
                    int dwm1w = 0;
                    int dataSize = 0;
                    bool align = false;
                    var spl1 = line.Split('#')[0].Split(',');
                    for (; i < spl1.Length; i++)
                    {
                        string param = spl1[i].Trim();
                        var spl2 = param.Split(':');
                        if (spl2.Length == 2)
                        {
                            if (pass == 1)
                            {
                                string label = spl2[0].Trim();
                                if (!Regex.Match(label, "\\A[a-zA-Z_][a-zA-Z0-9_]*\\Z").Success)
                                    throw new Exception();
                                labels.Add(label, ip);
                            }
                            param = spl2[1].Trim();
                        }
                        if (param.Length == 0 && spl1.Length == 1)
                            break;
                        if (pass == 2 && i == 0)
                            Console.Write($"{ip,5}: ");
                        var spl3 = param.Split(' ');
                        if (spl3[0].StartsWith(".d"))
                        {
                            if (dataSize != 0)
                                throw new Exception();
                            dataSize = int.Parse(spl3[0].Substring(2));
                            if (dataSize <= 0)
                                throw new Exception();
                            param = spl3[1].Trim();
                        }
                        else if (spl3[0].StartsWith(".align"))
                        {
                            if (align)
                                throw new Exception();
                            align = true;
                            long alignValue = Evaluate(ShuntingYard(TokenizeExpression(spl3[1])));
                            if (alignValue <= 1)
                                throw new Exception();
                            ulong mod = ip % (ulong)alignValue;
                            if (mod != 0)
                            {
                                dataSize = (int)((ulong)alignValue - mod);
                                ip += (ulong)dataSize;
                                if (pass == 2)
                                    WriteData(0, dataSize);
                            }
                            continue;
                        }
                        if (dataSize == 0)
                        {
                            if (i <= 1)
                            {
                                int v = int.Parse(param);
                                if (v <= 0)
                                    throw new Exception();
                                if (i == 0)
                                    aw = v;
                                else if (i == 1)
                                    dwm1w = v;
                                if (pass == 2)
                                    WriteGammaCode(v);
                                ip += GetGammaCodeLength(v);
                            }
                            else if (i == 2)
                            {
                                ip += (ulong)dwm1w;
                                if (pass == 2)
                                {
                                    int dwm1 = int.Parse(param);
                                    if (dwm1 < 0)
                                        throw new Exception();
                                    WriteData(dwm1, dwm1w);
                                }
                            }
                            else if (i >= 3 && i <= 5)
                            {
                                ip += (ulong)aw;
                                if (pass == 2)
                                {
                                    var tokens = ShuntingYard(TokenizeExpression(param));
                                    foreach (var token in tokens)
                                    {
                                        if (i == 5 && token.Value == "@n")
                                            token.IntValue = ip;
                                        else if (labels.ContainsKey(token.Value))
                                            token.IntValue = labels[token.Value];
                                    }
                                    WriteData(Evaluate(tokens), aw);
                                }
                            }
                            else
                                throw new Exception();
                        }
                        else if (!align)
                        {
                            ip += (ulong)dataSize;
                            if (pass == 2)
                            {
                                var tokens = ShuntingYard(TokenizeExpression(param));
                                foreach (var token in tokens)
                                    if (labels.ContainsKey(token.Value))
                                        token.IntValue = labels[param];
                                WriteData(Evaluate(tokens), dataSize);
                            }
                        }
                        else
                            throw new Exception();
                    }
                    if (!align)
                    {
                        if (dataSize == 0 && i > 0 && i != 6)
                            throw new Exception();
                        if (dataSize != 0 && i == 0)
                            throw new Exception();
                    }
                    if (pass == 2 && i != 0)
                        Console.WriteLine();
                }
            }
            File.WriteAllBytes(outputFile, obs.GetData());
        }

        static void ShowUsage()
        {
            Console.WriteLine(
                "usage: SubleqxAsm inputFile.sxa [-o outputFile.bin] [-b baseAddress]");
        }

        static void Main(string[] args)
        {
            System.Threading.Thread.CurrentThread.CurrentCulture =
                System.Globalization.CultureInfo.InvariantCulture;

            if (args.Length != 1 && args.Length != 3 && args.Length != 5)
            {
                ShowUsage();
                return;
            }

            string inputFile = args[0];
            string outputFile = Path.ChangeExtension(inputFile, ".bin");
            BigInteger baseAddr = BigInteger.Zero;

            for (int i = 1; i < args.Length; i += 2)
            {
                if (args[i] == "-o")
                    outputFile = args[i + 1];
                else if (args[i] == "-b")
                {
                    baseAddr = ParseInteger(args[i + 1]);
                    CheckInteger(baseAddr, 63);
                    if (baseAddr < 0)
                        throw new Exception();
                }
            }

            new Program().Assemble(inputFile, outputFile, (ulong)baseAddr);
        }
    }
}
