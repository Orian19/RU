namespace Ex03.GarageLogic
{
    public class Electricity : EnergySource
    {
        private float m_BatteryTimeRemaining;

        public Electricity(float i_MaxEnergyCapacity, float i_BatteryTimeRemaining) : base(i_MaxEnergyCapacity)
        {
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
                    throw new ValueOutOfRangeException(0f, r_MaxEnergyCapacity);
                }
            }
        }

        public void Recharge(float i_HoursChargeBattery)
        {
            if (m_BatteryTimeRemaining + i_HoursChargeBattery > base.r_MaxEnergyCapacity)
            {
                throw new ValueOutOfRangeException(0, base.r_MaxEnergyCapacity);
            }

            m_BatteryTimeRemaining += i_HoursChargeBattery;
        }
    }
}
