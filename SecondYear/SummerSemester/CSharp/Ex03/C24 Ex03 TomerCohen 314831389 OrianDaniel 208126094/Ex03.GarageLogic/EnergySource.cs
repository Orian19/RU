namespace Ex03.GarageLogic
{
    public abstract class EnergySource
    {
        protected readonly float r_MaxEnergyCapacity;

        public EnergySource(float i_MaxEnergyCapcity)
        {
            r_MaxEnergyCapacity = i_MaxEnergyCapcity;
        }

        public float MaxEnergyCapacity
        {
            get { return r_MaxEnergyCapacity; }
        }
    }
}
