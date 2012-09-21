using System;
using Microsoft.FSharp.Core;
using Errors = FSharpx.NonEmptyList<string>;

namespace FSharpx.CSharpTests {
    public class Person {
        public readonly string Name;
        public readonly int Age;

        private Person(string name, int age) {
            Name = name;
            Age = age;
        }

        public static readonly Lens<Person, string> NameLens =
            Lens.Create((Person x) => x.Name, (v, x) => new Person(v, x.Age));

        public static readonly Lens<Person, int> AgeLens =
            Lens.Create((Person x) => x.Age, (v, x) => new Person(x.Name, v));

        private static readonly Func<string, int, Person> New = (name, age) => new Person(name, age);

        public static FSharpChoice<Person, Errors> TryNew(string name, int age) {
            return New.Curry().ReturnValidation()
                .ApValidation(Mandatory(name))
                .ApValidation(Positive(age));
        }

        private static FSharpChoice<string, Errors> Mandatory(string s) {
            return FSharpChoice.Validator<string>(x => !string.IsNullOrEmpty(x), "Mandatory field")(s);
            //if (string.IsNullOrEmpty(s))
            //    return FSharpChoice.Error<string>("Mandatory field");
            //return FSharpChoice.Ok(s);
        }

        private static FSharpChoice<int, Errors> Positive(int a) {
            if (a <= 0)
                return FSharpChoice.Error<int>("Field must be positive");
            return FSharpChoice.Ok(a);
        }

    }
}