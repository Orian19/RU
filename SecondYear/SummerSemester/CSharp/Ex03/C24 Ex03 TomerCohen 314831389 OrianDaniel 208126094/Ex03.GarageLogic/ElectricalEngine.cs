using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Ex03.GarageLogic.FuelEngine;

namespace Ex03.GarageLogic
{
    public class ElectricalEngine : Engine
    {
        private readonly float r_MaxBatteryTime;
        private float m_BatteryTimeRemaining;

        protected ElectricalEngine(float i_MaxBatteryTime, float i_BatteryTimeRemaining)
        {
            r_MaxBatteryTime = i_MaxBatteryTime;
            m_BatteryTimeRemaining = i_BatteryTimeRemaining;
        }

        protected void ReCharge(float i_HoursChargeBattery)
        {
            if (m_BatteryTimeRemaining + i_HoursChargeBattery > r_MaxBatteryTime)
            {
                throw new ValueOutOfRangeException(0, r_MaxBatteryTime);
            }

            m_BatteryTimeRemaining += i_HoursChargeBattery;
        }
    }
}
