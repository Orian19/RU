using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Ex03.GarageLogic.Fuel;

namespace Ex03.GarageLogic
{
    public class Electricity : EnergySource
    {
        private float m_BatteryTimeRemaining;

        public Electricity(float i_MaxEnergyCapacity, float i_BatteryTimeRemaining) : base(i_MaxEnergyCapacity)
        {
            m_BatteryTimeRemaining = i_BatteryTimeRemaining;
        }

        protected void Recharge(float i_HoursChargeBattery)
        {
            if (m_BatteryTimeRemaining + i_HoursChargeBattery > base.r_MaxEnergyCapacity)
            {
                throw new ValueOutOfRangeException(0, base.r_MaxEnergyCapacity);
            }

            m_BatteryTimeRemaining += i_HoursChargeBattery;
        }
    }
}
