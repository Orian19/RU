using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex03.GarageLogic
{
    public class FuelEngine : Engine
    {
        private readonly float r_MaxFuelTank;
        private readonly eFuelType r_FuelType;
        private float m_CurrentFuel;

        public FuelEngine(eFuelType i_FuelType, float i_CurrentFuel, float i_MaxFuelTank)
        {
            r_FuelType = i_FuelType;
            m_CurrentFuel = i_CurrentFuel;
            r_MaxFuelTank = i_MaxFuelTank;
        }

        protected void ReFuel(float i_LiterFuel, eFuelType i_FuelType)
        {
            if (m_CurrentFuel + i_LiterFuel > r_MaxFuelTank)
            {
                throw new ValueOutOfRangeException(0, r_MaxFuelTank);
            }

            m_CurrentFuel += i_LiterFuel;
        }
    }
}
