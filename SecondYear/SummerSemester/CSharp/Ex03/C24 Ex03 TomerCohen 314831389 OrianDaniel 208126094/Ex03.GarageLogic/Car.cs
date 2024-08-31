namespace Ex03.GarageLogic
{
    public class Car : Vehicle
    {
        private readonly eColors r_Color;
        private readonly eDoors r_Doors;
        private const float k_MaxAirPressure = 33f;

        public Car(int i_NumOfWheels, string i_ModelName, string i_LicenseNumber, EnergySource i_Engine, eColors i_Color, eDoors i_Doors)
            : base(i_NumOfWheels, i_LicenseNumber, i_ModelName, i_Engine)
        {
            r_Color = i_Color;
            r_Doors = i_Doors;
        }

        public override string ToString()
        {
            return $@"
{base.ToString()}
Color: {r_Color}
Doors: {r_Doors}";
        }
    }
}
