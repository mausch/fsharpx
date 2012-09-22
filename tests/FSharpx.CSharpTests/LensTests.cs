using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class LensTests {
        private static readonly Person john = Person.TryNew("john", 55).Value();
        private static readonly Car bmw = new Car("BMW", john);

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

        [Test]
        public void Compose() {
            var carOwnerName = Car.OwnerLens.AndThen(Person.NameLens);
            Assert.AreEqual("john", carOwnerName.Get(bmw));
        }

        [Test]
        public void InstanceGet() {
            Assert.AreEqual(55, john.AgeL.Get());
        }

        [Test]
        public void InstanceSet() {
            var hector = john.NameL.Set("hector");
            Assert.AreEqual("hector", hector.Name);
        }

        [Test]
        public void InstanceUpdate() {
            var johnDoe = john.NameL.Update(x => x + " doe");
            Assert.AreEqual("john doe", johnDoe.Name);
        }

        [Test]
        public void InstanceCompose() {
            var bmwOwnerName = bmw.OwnerL.AndThen(Person.NameLens);
            var johnDoeBmw = bmwOwnerName.Update(x => x + " doe");
            Assert.AreEqual("john doe", johnDoeBmw.Owner.Name);
        }

        private class Car {
            public readonly string Make;
            public readonly Person Owner;

            public static readonly Lens<Car, string> MakeLens = 
                Lens.Create((Car x) => x.Make, (v,x) => new Car(v, x.Owner));

            public static readonly Lens<Car, Person> OwnerLens =
                Lens.Create((Car x) => x.Owner, (v, x) => new Car(x.Make, v));

            public readonly InstanceLens<Car, Person> OwnerL;

            public Car(string make, Person owner) {
                Make = make;
                Owner = owner;
                OwnerL = InstanceLens.Create(this, OwnerLens);
            }
        }
    }
}
