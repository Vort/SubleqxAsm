using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
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
            if (value >= 1L << size)
                throw new Exception();
            for (int i = size - 1; i > -1; i--)
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

        public int GetSize()
        {
            if (bitIndex != 0)
                return data.Count + 1;
            else
                return data.Count;
        }

        public byte[] GetData()
        {
            if (bitIndex != 0)
                data.Add(byteValue);
            return data.ToArray();
        }
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
            obs.WriteValue(value, size);
            Console.Write($"{value} ");
        }

        void WriteGammaCode(int value)
        {
            if (value <= 0)
                throw new Exception();
            int sl = GetSizeLog(value);
            obs.WriteValue(0, sl - 1);
            obs.WriteValue(value, sl);
            Console.Write($"{value} ");
        }

        uint GetGammaCodeLength(int value)
        {
            if (value <= 0)
                throw new Exception();
            int sl = GetSizeLog(value);
            return (uint)(2 * sl - 1);
        }

        static bool ParseInteger(string s, int width, ref long value)
        {
            string st = s;
            bool negative = false;
            if (st.StartsWith("-"))
            {
                negative = true;
                st = st.Substring(1);
            }
            bool hex = false;
            if (st.StartsWith("0x"))
            {
                hex = true;
                st = "0" + st.Substring(2);
            }
            BigInteger n;
            if (!BigInteger.TryParse(st, hex ? NumberStyles.HexNumber : 0, null, out n))
                return false;
            BigInteger limit = BigInteger.One;
            limit <<= width;
            if (n >= limit)
                return false;
            value = (long)(negative ? -n : n);
            return true;
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
                        if (pass == 2 && i == 0)
                            Console.Write($"{ip,5}: ");
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
                                    long p = 0;
                                    if (!ParseInteger(param, aw, ref p))
                                    {
                                        if (i == 5 && param == "@n")
                                            p = (long)ip;
                                        else if (labels.ContainsKey(param))
                                            p = (long)labels[param];
                                        else
                                            throw new Exception($"{param} label is not found");
                                    }
                                    WriteData(p, aw);
                                }
                            }
                            else
                                throw new Exception();
                        }
                        else
                        {
                            ip += (ulong)dataSize;
                            if (pass == 2)
                            {
                                long d = 0;
                                if (!ParseInteger(param, dataSize, ref d))
                                    throw new Exception();
                                WriteData(d, dataSize);
                            }
                        }
                    }
                    if (dataSize == 0 && i > 0 && i != 6)
                        throw new Exception();
                    if (dataSize != 0 && i == 0)
                        throw new Exception();
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
            long baseAddr = 0;

            for (int i = 1; i < args.Length; i += 2)
            {
                if (args[i] == "-o")
                    outputFile = args[i + 1];
                else if (args[i] == "-b")
                {
                    if (!ParseInteger(args[i + 1], 63, ref baseAddr))
                        throw new Exception();
                    if (baseAddr < 0)
                        throw new Exception();
                }
            }

            new Program().Assemble(inputFile, outputFile, (ulong)baseAddr);
        }
    }
}
