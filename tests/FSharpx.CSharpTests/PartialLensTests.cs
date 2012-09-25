using System;
using Microsoft.FSharp.Core;
using NUnit.Framework;
using FSharpx;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class PartialLensTests {
        private class StateCity {
            public readonly string State;
            public readonly FSharpOption<string> City;

            public static readonly PartialLens<StateCity, string> CityP =
                PartialLensEx.Create((StateCity x) => x.City.Select(city => Tuple.Create<Func<string, StateCity>, string>(n => new StateCity(x.State, n.Some()), city)));

            public StateCity(string state, FSharpOption<string> city) {
                State = state;
                City = city;
            }
        }

        private class Search {
            public readonly FSharpOption<string> Name;
            public readonly FSharpOption<StateCity> StateCity;

            public static readonly PartialLens<Search, StateCity> StateCityP =
                PartialLensEx.Create((Search x) => x.StateCity.Select(city => Tuple.Create<Func<StateCity, Search>, StateCity>(n => new Search(x.Name, n.Some()), city)));

            public static readonly PartialLens<Search, string> CityP = StateCityP.AndThen(PartialLensTests.StateCity.CityP);

            public readonly InstancePartialLens<Search, string> ICityP;

            public Search(FSharpOption<string> name, FSharpOption<StateCity> stateCity) {
                Name = name;
                StateCity = stateCity;
                ICityP = InstancePartialLens.Create(this, CityP);
            }
        }

        [Test]
        public void SetNoState() {
            var noState = new Search("John".Some(), null);
            var orlando = noState.ICityP.Set("Orlando");
            Assert.IsNull(orlando.StateCity);
        }
    }
}
