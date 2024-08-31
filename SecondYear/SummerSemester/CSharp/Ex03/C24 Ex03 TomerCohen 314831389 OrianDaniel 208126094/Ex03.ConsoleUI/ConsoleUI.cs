using Ex03.GarageLogic;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex03.ConsoleUI
{
    public class ConsoleUI
    {
        private readonly Garage r_Garage;

        public ConsoleUI()
        {
            r_Garage = new Garage();
        }

        private int displayMenuAndGetChoice()
        {
            const int minOption = 1;
            const int maxOption = 8;
            int userChoice = -1;
            bool isValidChoice;

            string menu = $@"
Welcome to the best garage!

Please choose an option:
1. Enter a new vehicle into the garage.
2. Show license numbers of vehicles by filter.
3. Change the status of a vehicle.
4. Inflate air in wheels.
5. Fill gas tank.
6. Charge battery.
7. Show full vehicle details by license number.
8. Exit garage
";

            Console.WriteLine(menu);

            do
            {
                Console.Write("Your choice: ");
                string input = Console.ReadLine();

                try
                {
                    if (!int.TryParse(input, out userChoice))
                    {
                        throw new FormatException("Input must be an integer.");
                    }

                    if (userChoice < minOption || userChoice > maxOption)
                    {
                        throw new ArgumentOutOfRangeException($"Choice must be between {minOption} and {maxOption}.");
                    }

                    isValidChoice = true;
                }
                catch (FormatException ex)
                {
                    Console.WriteLine($"Invalid input: {ex.Message}");
                    isValidChoice = false;
                }
                catch (ArgumentOutOfRangeException ex)
                {
                    Console.WriteLine($"Invalid input: {ex.Message}");
                    isValidChoice = false;
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"An unexpected error occurred: {ex.Message}");
                    isValidChoice = false;
                }

            } while (!isValidChoice);

            return userChoice;
        }

        public void GarageManager()
        {
            bool quit = false;
            int userChoice;

            while (!quit)
            {
                userChoice = displayMenuAndGetChoice();

                switch (userChoice)
                {
                    case 1:
                        AddVehicle();
                        break;
                    case 2:
                        displayLicensesByFilter();
                        break;
                    case 3:
                        changeVehicleState();
                        break;
                    case 4:
                        inflateWheels();
                        break;
                    case 5:
                        fillGasTank();
                        break;
                    case 6:
                        chargeBattery();
                        break;
                    case 7:
                        showVehicleDetails();
                        break;
                    case 8:
                        quit = true;
                        break;
                    default:
                        throw new ArgumentException("Choice not valid");
                }
            }
        }

        private string getLicenseNumber()
        {
            Console.WriteLine("Enter vehicle license number:");
            return Console.ReadLine();
        }

        private void showVehicleDetails()
        {
            string licenseNumber = getLicenseNumber();
            r_Garage.DisplayFullVehicleInfo(licenseNumber);
        }

        private void chargeBattery()
        {
            string licenseNumber = getLicenseNumber();

            Console.WriteLine("Enter number of minutes to charge:");
            if (!float.TryParse(Console.ReadLine(), out float minutesToCharge))
            {
                throw new FormatException("invalid minutes input");
            }

            r_Garage.RechargeVehicle(licenseNumber, minutesToCharge);
        }

        private void fillGasTank()
        {
            string licenseNumber = getLicenseNumber();

            int maxChoice = Enum.GetValues(typeof(eFuelType)).Length;
            Console.WriteLine("Choose a fuel type for the vehicle:");
            DisplayEnumValues<eFuelType>();

            int fuelType;
            while (!int.TryParse(Console.ReadLine(), out fuelType) || fuelType < 1 || fuelType > maxChoice)
            {
                Console.WriteLine($"Please enter a valid choice between 1 and {maxChoice}.");
                Console.WriteLine("Choose a fuel type for the vehicle:");
                DisplayEnumValues<eFuelType>();
            }

            eFuelType fuelTypeEnum = (eFuelType)(fuelType - 1);

            Console.WriteLine("Enter amount of fuel for the refuel:");
            if(!float.TryParse(Console.ReadLine(), out float fuelAmount))
            {
                throw new FormatException("invalid fuel input");
            }

            r_Garage.RefuelVehicle(licenseNumber, fuelTypeEnum, fuelAmount);
        }

        private void inflateWheels()
        {
            string licenseNumber = getLicenseNumber();

            r_Garage.InflateToMax(licenseNumber);
        }

        private void changeVehicleState()
        {
            string licenseNumber = getLicenseNumber();

            int maxChoice = Enum.GetValues(typeof(eVehicleState)).Length;
            Console.WriteLine("Choose a status for the vehicle:");
            DisplayEnumValues<eVehicleState>();

            int vehicleState;
            while (!int.TryParse(Console.ReadLine(), out vehicleState) || vehicleState < 1 || vehicleState > maxChoice)
            {
                Console.WriteLine($"Please enter a valid choice between 1 and {maxChoice}.");
                Console.WriteLine("Choose one of our vehicle states:");
                DisplayEnumValues<eVehicleState>();
            }

            eVehicleState vehicleStateEnum = (eVehicleState)(vehicleState - 1);
            r_Garage.ChangeVehicleState(licenseNumber, vehicleStateEnum);
        }

        private void displayLicensesByFilter()
        {
            bool inRepair = GetYesOrNo("Do you want to see vehicles in repair? (Y/N): ");
            bool repaired = GetYesOrNo("Do you want to see vehicles that are repaired? (Y/N): ");
            bool paid = GetYesOrNo("Do you want to see vehicles that are paid? (Y/N): ");

            r_Garage.DisplayListOfLicenseNumbers(inRepair, repaired, paid);
        }

        private bool GetYesOrNo(string i_UserInput)
        {
            string choice;

            do
            {
                Console.Write(i_UserInput);
                choice = Console.ReadLine();
            } while (choice != "Y" && choice != "N");

            return choice == "Y";
        }

        private void AddVehicle()
        {
            OwnerInfo ownerInfo = getOwnerInfo();

            if (r_Garage.AddVehicle(ownerInfo))
            {
                Console.WriteLine(ownerInfo.ToString());
                Console.WriteLine("This vehicle added successfuly to garage");
            }
            else
            {
                Console.WriteLine("This vehicle is already in the garage");
            }
        }

        private OwnerInfo getOwnerInfo()
        {
            Console.WriteLine("Please enter customer's name:");
            string name = Console.ReadLine();

            Console.WriteLine("Please enter customer's phone number:");
            string phoneNumber = Console.ReadLine();       

            Vehicle vehicle = getVehicleInfo();

            return new OwnerInfo(name, phoneNumber, eVehicleState.InRepair, vehicle);
        }

        private Vehicle getVehicleInfo()
        {
            string licenseNumber = getLicenseNumber();

            Console.WriteLine("Please enter model name:");
            string modelName = Console.ReadLine();

            int maxChoice = Enum.GetValues(typeof(eVehiclesTypes)).Length;
            Console.WriteLine("Choose one of our vehicles:");
            DisplayEnumValues<eVehiclesTypes>();

            int vehicleType;
            while (!int.TryParse(Console.ReadLine(), out vehicleType) || vehicleType < 1 || vehicleType > maxChoice)
            {
                Console.WriteLine($"Please enter a valid choice between 1 and {maxChoice}.");
                Console.WriteLine("Choose one of our vehicles:");
                DisplayEnumValues<eVehiclesTypes>();
            }

            eVehiclesTypes vehicleTypeEnum = (eVehiclesTypes)(vehicleType - 1);
            Dictionary<eOptions, object> options = r_Garage.GetOptions(vehicleTypeEnum);



            return r_Garage.CreateVehicle(vehicleTypeEnum, licenseNumber, options, modelName);
        }

        private void DisplayEnumValues<T>() where T : Enum
        {
            int index = 1;
            foreach (T value in Enum.GetValues(typeof(T)))
            {
                Console.WriteLine($"{index++}. {value}");
            }
        }
    }
}
