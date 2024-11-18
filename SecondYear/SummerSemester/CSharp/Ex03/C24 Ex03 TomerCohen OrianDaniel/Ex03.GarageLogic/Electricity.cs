using System;

namespace Ex03.GarageLogic
{
    public class Electricity : EnergySource
    {
        private float m_BatteryTimeRemaining;

        public Electricity(float i_MaxEnergyCapacity, float i_BatteryTimeRemaining) : base(i_MaxEnergyCapacity)
        {
            if (i_BatteryTimeRemaining < 0 || i_BatteryTimeRemaining > MaxEnergyCapacity)
            {
                Exception ex = new Exception("Invalid input for battery time remaining.");
                throw new ValueOutOfRangeException(0f, r_MaxEnergyCapacity, ex);
            }

            m_BatteryTimeRemaining = i_BatteryTimeRemaining;
        }

        public float BatteryTimeRemaining
        {
            get { return m_BatteryTimeRemaining; }
            set
            {
                if (value >= 0 && value <= r_MaxEnergyCapacity)
                {
                    m_BatteryTimeRemaining = value;
                }
                else
                {
                    Exception ex = new Exception("Invalid input for battery time remaining.");
                    throw new ValueOutOfRangeException(0f, r_MaxEnergyCapacity, ex);
                }
            }
        }

        public void Recharge(float i_HoursChargeBattery)
        {
            if (m_BatteryTimeRemaining + i_HoursChargeBattery > r_MaxEnergyCapacity)
            {
                Exception ex = new Exception("Invalid input for the number of hours to charge.");
                throw new ValueOutOfRangeException(0, r_MaxEnergyCapacity - m_BatteryTimeRemaining, ex);
            }

            m_BatteryTimeRemaining += i_HoursChargeBattery;
        }

        public override string ToString()
        {
            return $"The battery has {BatteryTimeRemaining} hours remaining out of a total capacity of {r_MaxEnergyCapacity} hours.";
        }
    }
}
