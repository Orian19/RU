using System;

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

        public eFuelType FuelType
        {
            get { return r_FuelType; }
        }

        public float CurrentFuel
        {
            get { return m_CurrentFuel; }
            set
            {
                if (value >= 0 && value <= r_MaxEnergyCapacity)
                {
                    m_CurrentFuel = value;
                }
                else
                {
                    throw new ValueOutOfRangeException(0f, r_MaxEnergyCapacity);
                }
            }
        }

        public void Refuel(float i_LiterToAdd, eFuelType i_FuelType)
        {
            if (i_FuelType.Equals(FuelType))
            {
                if (i_LiterToAdd + m_CurrentFuel <= r_MaxEnergyCapacity)
                {
                    m_CurrentFuel += i_LiterToAdd;
                }
                else
                {
                    throw new ValueOutOfRangeException(0f, r_MaxEnergyCapacity);
                }
            }
            else
            {
                throw new ArgumentException("Fuel Type does not match");
            }
        }
    }
}
