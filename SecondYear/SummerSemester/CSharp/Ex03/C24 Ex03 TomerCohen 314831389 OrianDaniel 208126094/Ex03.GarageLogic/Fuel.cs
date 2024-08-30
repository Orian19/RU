using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex03.GarageLogic
{
    public class Fuel : EnergySource
    {
        private readonly eFuelType r_FuelType;
        private float m_CurrentFuel;

        public Fuel(float i_MaxEnergyCapacity, eFuelType i_FuelType, float i_CurrentFuel) : base(i_MaxEnergyCapacity)
        {
            r_FuelType = i_FuelType;
            m_CurrentFuel = i_CurrentFuel;
        }

        protected void Refuel(float i_LiterFuel, eFuelType i_FuelType)
        {
            if (m_CurrentFuel + i_LiterFuel > base.r_MaxEnergyCapacity)
            {
                throw new ValueOutOfRangeException(0, base.r_MaxEnergyCapacity);
            }

            m_CurrentFuel += i_LiterFuel;
        }
    }
}
