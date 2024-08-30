using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex03.GarageLogic
{
    public abstract class EnergySource
    {
        protected readonly float r_MaxEnergyCapacity;

        public EnergySource(float i_MaxEnergyCapcity)
        {
            r_MaxEnergyCapacity = i_MaxEnergyCapcity;
        }
        public float OwnerName
        {
            get { return r_MaxEnergyCapacity; }
        }
    }
}
