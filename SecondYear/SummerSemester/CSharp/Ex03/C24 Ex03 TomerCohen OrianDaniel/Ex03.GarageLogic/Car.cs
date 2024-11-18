namespace Ex03.GarageLogic
{
    public class Car : Vehicle
    {
        private readonly eColors r_Color;
        private readonly eDoors r_Doors;

        public Car(int i_NumOfWheels, string i_ModelName, string i_LicenseNumber, EnergySource i_Energy, eColors i_Color, eDoors i_Doors)
            : base(i_NumOfWheels, i_ModelName, i_LicenseNumber, i_Energy)
        {
            r_Color = i_Color;
            r_Doors = i_Doors;
        }

        public eColors Colors
        {
            get { return r_Color; }
        }

        public eDoors Doors
        {
            get { return r_Doors; }
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
