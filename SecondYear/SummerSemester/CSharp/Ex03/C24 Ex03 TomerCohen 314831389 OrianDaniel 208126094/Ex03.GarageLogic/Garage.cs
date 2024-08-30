using System;
using System.Collections.Generic;

namespace Ex03.GarageLogic
{
    public class Garage
    {
        private readonly Dictionary<string, OwnerInfo> r_Vehicles;

        public Garage()
        {
            r_Vehicles = new Dictionary<string, OwnerInfo>();
        }

        public void AddVehicle(OwnerInfo i_OwnerInfo)
        {
            if (r_Vehicles.ContainsKey(i_OwnerInfo.Vehicle.LicenseNumber))
            {
                try
                {
                    i_OwnerInfo.VehicleState = eVehicleState.InRepair;
                }
                catch (Exception ex)
                {
                    throw new Exception($"License number {i_OwnerInfo.Vehicle.LicenseNumber} is not in the system.", ex);
                }
            }
            else
            {
                r_Vehicles.Add(i_OwnerInfo.Vehicle.LicenseNumber, i_OwnerInfo);
            }
        }

        public static Vehicle CreateVehicle(eVehiclesTypes i_VehiclesChoice, string i_LicenseNumber)
        {
            Vehicle newVehicle = null;

            switch (i_VehiclesChoice)
            {
                case eVehiclesTypes.RegularBike:
                    {
                        Fuel engine = new Fuel(6, eFuelType.Octan98, 7f);
                        return new Bike(i_LicenseNumber, 2, 30, engine);
                        break;
                    }

                case eVehiclesTypes.ElectricBike:
                    {

                        break;
                    }

                case eVehiclesTypes.RegularCar:
                    {

                        break;
                    }

                case eVehiclesTypes.ElectricCar:
                    {

                        break;
                    }

                case eVehiclesTypes.Truck:
                    {

                        break;
                    }
            }

            return newVehicle;
        }

        public void DisplayListOfLicesnseNumbers()
        {
            return;
        }

        public void ChangeVehicleState(string i_LicenseNumber, eVehicleState i_VehicleState)
        {
            if (r_Vehicles.TryGetValue(i_LicenseNumber, out OwnerInfo ownerInfo))
            {
               ownerInfo.VehicleState = i_VehicleState;
            }
            else
            {
                throw new ArgumentException("No vehicle with this license number was found");
            }
        }

        public void InflateToMax(string i_LicenseNumber)
        {
            if (r_Vehicles.TryGetValue(i_LicenseNumber, out OwnerInfo ownerInfo))
            {
                foreach (var wheel in ownerInfo.Vehicle.Wheels)
                {
                    wheel.inflate(wheel.MaxAirPressure - wheel.CurrentAirPressure);
                }
            }
            else
            {
                throw new ArgumentException("No vehicle with this license number was found");
            }
        }

        public void RefuelVehicle(string i_LicenseNumber, eFuelType i_FuelType, float i_FuelAmount)
        {
            if (r_Vehicles.TryGetValue(i_LicenseNumber, out OwnerInfo ownerInfo))
            {
                if ()
            }
            else
            {
                throw new ArgumentException("No vehicle with this license number was found");
            }
        }

        public void RechargeVehicle(string  i_LicenseNumber, float i_minutesToCharge)
        {
            if (r_Vehicles.TryGetValue(i_LicenseNumber, out OwnerInfo ownerInfo))
            {
                if()
            }
            else
            {
                throw new ArgumentException("No vehicle with this license number was found");
            }
        }

        public void DisplayFullVehicleInfo(string i_LicenseNumber)
        {
            if (r_Vehicles.TryGetValue(i_LicenseNumber, out OwnerInfo ownerInfo))
            {
                Console.WriteLine(ownerInfo.ToString());
            }
            else
            {
                throw new ArgumentException("No vehicle with this license number was found");
            }
        }
    }
}
