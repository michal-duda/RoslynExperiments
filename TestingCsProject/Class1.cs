using System;

namespace TestingCsProject
{

    public class LoggingAttribute : Attribute
    {
    }

    public class Class1
    {
        [Logging]
        public void TestMethod(int arg1, string arg2)
        {
            Console.WriteLine("Hello!");
        }
    }
}
