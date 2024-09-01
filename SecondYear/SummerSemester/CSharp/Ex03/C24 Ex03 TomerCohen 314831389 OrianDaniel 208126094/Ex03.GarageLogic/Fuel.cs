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

            if (i_CurrentFuel < 0 || i_CurrentFuel > i_MaxEnergyCapacity)
            {
                Exception ex = new Exception("Invalid input for current fuel.");
                throw new ValueOutOfRangeException(0f, i_MaxEnergyCapacity, ex);
            }

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
                    Exception ex = new Exception("Invalid input for current fuel.");
                    throw new ValueOutOfRangeException(0f, r_MaxEnergyCapacity, ex);
                }
            }
        }

        public void Refuel(float i_FuelToAdd, eFuelType i_FuelType)
        {
            if (i_FuelType.Equals(FuelType))
            {
                if (i_FuelToAdd + m_CurrentFuel <= r_MaxEnergyCapacity)
                {
                    m_CurrentFuel += i_FuelToAdd;
                }
                else
                {
                    Exception ex = new Exception("Invalid input for the amount of fuel to add.");
                    throw new ValueOutOfRangeException(0f, r_MaxEnergyCapacity - m_CurrentFuel, ex);
                }
            }
            else
            {
                throw new ArgumentException("Fuel Type does not match");
            }
        }

        public override string ToString()
        {
            return $"Fuel type: {FuelType}, with {CurrentFuel} units remaining out of a maximum capacity of {r_MaxEnergyCapacity}.";
        }
    }
}