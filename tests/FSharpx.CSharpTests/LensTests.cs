using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class LensTests {
        private static readonly Person john = Person.TryNew("john", 55).Value();

        [Test]
        public void Get() {
            Assert.AreEqual("john", Person.NameLens.Get(john));
            Assert.AreEqual(55, Person.AgeLens.Get(john));
        }

        [Test]
        public void Set() {
            var hector = Person.NameLens.Set(john, "hector");
            Assert.AreEqual("hector", hector.Name);
        }

        [Test]
        public void Update() {
            var johnDoe = Person.NameLens.Update(john, x => x + " doe");
            Assert.AreEqual("john doe", johnDoe.Name);
        }
    }
}
