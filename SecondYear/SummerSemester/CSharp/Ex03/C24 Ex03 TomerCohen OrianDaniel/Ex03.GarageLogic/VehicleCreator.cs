using System;
using System.Collections.Generic;

namespace Ex03.GarageLogic
{
    public class VehicleCreator
    {
        public Vehicle CreateVehicle(eVehiclesTypes i_VehicleType, string i_LicenseNumber, Dictionary<eOptions, object> i_Options, string i_ModelName)
        {
            Vehicle newVehicle = null;
            EnergySource energySource = null;
            int numberOfWheels = 0;
            float maxAirPressure = 0;

            try
            {
                if (!i_Options.TryGetValue(eOptions.ManufacturerWheelName, out object manufacturerNameObj) || !(manufacturerNameObj is string manufacturerName) ||
                    !i_Options.TryGetValue(eOptions.CurrentWheelPressure, out object currentWheelPressureObj) || !(currentWheelPressureObj is float currentWheelPressure))
                {
                    throw new ArgumentException("Required wheel options are missing or invalid.");
                }

                switch (i_VehicleType)
                {
                    case eVehiclesTypes.RegularBike:
                        if (i_Options.TryGetValue(eOptions.CurrentFuel, out object bikeFuelObj) && bikeFuelObj is float bikeFuel &&
                            i_Options.TryGetValue(eOptions.LicenseType, out object bikeLicenseTypeObj) && bikeLicenseTypeObj is eLicenseType bikeLicenseType &&
                            i_Options.TryGetValue(eOptions.EngineVolume, out object bikeEngineVolObj) && bikeEngineVolObj is int bikeEngineVolume)
                        {
                            energySource = new Fuel(6, eFuelType.Octan98, bikeFuel);
                            newVehicle = new Bike(2, i_ModelName, i_LicenseNumber, energySource, bikeLicenseType, bikeEngineVolume);
                            energySource.RemainingEnergyPercentage = (bikeFuel / energySource.MaxEnergyCapacity) * 100;
                            numberOfWheels = 2;
                            maxAirPressure = 31;
                        }
                        break;

                    case eVehiclesTypes.ElectricBike:
                        if (i_Options.TryGetValue(eOptions.BatteryTimeRemaining, out object bikeBatteryObj) && bikeBatteryObj is float bikeBatteryTime &&
                            i_Options.TryGetValue(eOptions.LicenseType, out object eBikeLicenseTypeObj) && eBikeLicenseTypeObj is eLicenseType eBikeLicenseType &&
                            i_Options.TryGetValue(eOptions.EngineVolume, out object eBikeEngineVolObj) && eBikeEngineVolObj is int eBikeEngineVolume)
                        {
                            energySource = new Electricity(2.7f, bikeBatteryTime);
                            newVehicle = new Bike(2, i_ModelName, i_LicenseNumber, energySource, eBikeLicenseType, eBikeEngineVolume);
                            energySource.RemainingEnergyPercentage = (bikeBatteryTime / energySource.MaxEnergyCapacity) * 100;
                            numberOfWheels = 2;
                            maxAirPressure = 31;
                        }
                        break;

                    case eVehiclesTypes.RegularCar:
                        if (i_Options.TryGetValue(eOptions.CurrentFuel, out object carFuelObj) && carFuelObj is float carFuel &&
                            i_Options.TryGetValue(eOptions.Color, out object carColorObj) && carColorObj is eColors carColor &&
                            i_Options.TryGetValue(eOptions.Doors, out object carDoorsObj) && carDoorsObj is eDoors carDoors)
                        {
                            energySource = new Fuel(49, eFuelType.Octan95, carFuel);
                            newVehicle = new Car(5, i_ModelName, i_LicenseNumber, energySource, carColor, carDoors);
                            energySource.RemainingEnergyPercentage = (carFuel / energySource.MaxEnergyCapacity) * 100;
                            numberOfWheels = 5;
                            maxAirPressure = 33;
                        }
                        break;

                    case eVehiclesTypes.ElectricCar:
                        if (i_Options.TryGetValue(eOptions.BatteryTimeRemaining, out object carBatteryObj) && carBatteryObj is float carBatteryTime &&
                            i_Options.TryGetValue(eOptions.Color, out object eCarColorObj) && eCarColorObj is eColors eCarColor &&
                            i_Options.TryGetValue(eOptions.Doors, out object eCarDoorsObj) && eCarDoorsObj is eDoors eCarDoors)
                        {
                            energySource = new Electricity(5f, carBatteryTime);
                            newVehicle = new Car(5, i_ModelName, i_LicenseNumber, energySource, eCarColor, eCarDoors);
                            energySource.RemainingEnergyPercentage = (carBatteryTime / energySource.MaxEnergyCapacity) * 100;
                            numberOfWheels = 5;
                            maxAirPressure = 33;
                        }
                        break;

                    case eVehiclesTypes.Truck:
                        if (i_Options.TryGetValue(eOptions.CurrentFuel, out object truckFuelObj) && truckFuelObj is float truckFuel &&
                            i_Options.TryGetValue(eOptions.CarryDangerousMaterialsDang, out object truckDangerObj) && truckDangerObj is bool truckDangerous &&
                            i_Options.TryGetValue(eOptions.CargoVolume, out object truckCargoObj) && truckCargoObj is float truckCargo)
                        {
                            energySource = new Fuel(130, eFuelType.Soler, truckFuel);
                            newVehicle = new Truck(14, i_ModelName, i_LicenseNumber, energySource, truckDangerous, truckCargo);
                            energySource.RemainingEnergyPercentage = (truckFuel / energySource.MaxEnergyCapacity) * 100;
                            numberOfWheels = 14;
                            maxAirPressure = 28;
                        }
                        break;

                    default:
                        throw new ArgumentException("Unknown vehicle type.");
                }

                for (int i = 0; i < numberOfWheels; i++)
                {
                    Wheel wheel = new Wheel(manufacturerName, maxAirPressure);
                    wheel.CurrentAirPressure = currentWheelPressure;
                    newVehicle.Wheels.Add(wheel);
                }
            }
            catch (Exception ex)
            {
                if (ex is ValueOutOfRangeException)
                {
                    Console.WriteLine((ex as ValueOutOfRangeException).InnerException.Message);
                }

                throw new InvalidOperationException($"{ex.Message}", ex);
            }

            return newVehicle;
        }

        public Dictionary<eOptions, object> GetOptions(eVehiclesTypes i_VehicleType)
        {
            Dictionary<eOptions, object> options = new Dictionary<eOptions, object>();

            options.Add(eOptions.CurrentWheelPressure, typeof(float));
            options.Add(eOptions.ManufacturerWheelName, typeof(string));

            switch (i_VehicleType)
            {
                case eVehiclesTypes.RegularBike:
                    addBikeOptions(options);
                    addFuelOptions(options);
                    break;

                case eVehiclesTypes.ElectricBike:
                    addBikeOptions(options);
                    addElectricityOptions(options);
                    break;

                case eVehiclesTypes.RegularCar:
                    addCarOptions(options);
                    addFuelOptions(options);
                    break;

                case eVehiclesTypes.ElectricCar:
                    addCarOptions(options);
                    addElectricityOptions(options);
                    break;

                case eVehiclesTypes.Truck:
                    options.Add(eOptions.CarryDangerousMaterialsDang, typeof(bool));
                    options.Add(eOptions.CargoVolume, typeof(float));
                    addFuelOptions(options);
                    break;
            }

            return options;
        }

        private void addBikeOptions(Dictionary<eOptions, object> i_Options)
        {
            i_Options.Add(eOptions.LicenseType, typeof(eLicenseType));
            i_Options.Add(eOptions.EngineVolume, typeof(int));
        }

        private void addCarOptions(Dictionary<eOptions, object> i_Options)
        {
            i_Options.Add(eOptions.Color, typeof(eColors));
            i_Options.Add(eOptions.Doors, typeof(eDoors));
        }

        private void addFuelOptions(Dictionary<eOptions, object> i_Options)
        {
            i_Options.Add(eOptions.FuelType, typeof(eFuelType));
            i_Options.Add(eOptions.CurrentFuel, typeof(float));
            i_Options.Add(eOptions.MaxFuel, typeof(float));
        }

        private void addElectricityOptions(Dictionary<eOptions, object> i_Options)
        {
            i_Options.Add(eOptions.BatteryTimeRemaining, typeof(float));
            i_Options.Add(eOptions.MaxBatteryTime, typeof(float));
        }
    }
}
