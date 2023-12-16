package co.com.sura.datatest.reportes;

import co.com.sura.reportes.entity.cancelacioncitas.RegistroCancelacionCita;
import co.com.sura.reportes.entity.cancelacioncitas.RegistroCancelacionCitaAnual;
import co.com.sura.reportes.entity.cancelacioncitas.RegistroCancelacionCitaMensual;
import co.com.sura.reportes.entity.turnos.ItemReporteAnual;
import co.com.sura.reportes.entity.turnos.ItemReporteMensual;

import java.util.ArrayList;
import java.util.List;

public class ReportesTestData {
    public String numeroMesTest;
    public String nombreMesTest;
    public String mesTestEsperado;
    public String diaTest1;
    public String diaTest2;
    public ItemReporteAnual itemReporteAnualTest1;
    public ItemReporteAnual itemReporteAnualTest2;
    public ItemReporteMensual itemReporteMesualTest1;
    public ItemReporteMensual itemReporteMensualTest2;

    public RegistroCancelacionCitaAnual registroValidarNombreMes;
    public RegistroCancelacionCitaAnual registroCancelacionAnualCita1;
    public RegistroCancelacionCitaAnual registroCancelacionAnualCita2;
    public RegistroCancelacionCitaAnual registroCancelacionAnualCita3;
    public RegistroCancelacionCitaAnual registroCancelacionAnualCita4;

    public RegistroCancelacionCitaMensual registroCancelacionMensualCita1;
    public RegistroCancelacionCitaMensual registroCancelacionMensualCita2;
    public RegistroCancelacionCitaMensual registroCancelacionMensualCita3;
    public RegistroCancelacionCitaMensual registroCancelacionMensualCita4;
    public List<RegistroCancelacionCita> registrosCancelacion1;
    public List<RegistroCancelacionCita> registrosCancelacion2;

    public ReportesTestData() {
        numeroMesTest = "5";
        nombreMesTest = "mayo";
        mesTestEsperado = "marzo";
        diaTest1 = "1";
        diaTest2 = "2";
        itemReporteAnualTest1 = ItemReporteAnual
                .builder()
                .capacidadPromedio(50.0)
                .totalHorasAtencionesCompletadas(1000.0)
                .totalCitasCompletadas(100)
                .totalCitasCanceladas(10)
                .totalNovedades(200)
                .totalRemisiones(100)
                .cumplimientoCitasPromedio(80.0)
                .mes(numeroMesTest)
                .build();
        itemReporteAnualTest2 = ItemReporteAnual
                .builder()
                .capacidadPromedio(60.0)
                .totalHorasAtencionesCompletadas(1000.0)
                .totalCitasCompletadas(100)
                .totalCitasCanceladas(10)
                .totalNovedades(200)
                .totalRemisiones(100)
                .cumplimientoCitasPromedio(90.0)
                .mes(numeroMesTest)
                .build();

        itemReporteMesualTest1 = ItemReporteMensual
                .builder()
                .capacidadPromedio(50.0)
                .totalHorasAtencionesCompletadas(1000.0)
                .totalCitasCompletadas(100)
                .totalCitasCanceladas(10)
                .totalNovedades(200)
                .totalRemisiones(100)
                .cumplimientoCitasPromedio(80.0)
                .dia(diaTest1)
                .build();

        itemReporteMensualTest2 = ItemReporteMensual
                .builder()
                .capacidadPromedio(60.0)
                .totalHorasAtencionesCompletadas(1000.0)
                .totalCitasCompletadas(100)
                .totalCitasCanceladas(10)
                .totalNovedades(200)
                .totalRemisiones(100)
                .cumplimientoCitasPromedio(90.0)
                .dia(diaTest2)
                .build();

        registrosCancelacion1 = new ArrayList<>();
        registrosCancelacion1.add(new RegistroCancelacionCita("d1",20));

        registrosCancelacion2 = new ArrayList<>();
        registrosCancelacion2.add(new RegistroCancelacionCita("d4",20));

        registroValidarNombreMes = RegistroCancelacionCitaAnual
                .builder()
                .mes("3")
                .totalCitasCanceladas(25)
                .build();

        registroCancelacionAnualCita1 = RegistroCancelacionCitaAnual
                .builder()
                .mes("2")
                .totalCitasCanceladas(20)
                .registros(registrosCancelacion1)
                .build();
        registroCancelacionAnualCita2 = RegistroCancelacionCitaAnual
                .builder()
                .mes("12")
                .totalCitasCanceladas(40)
                .registros(registrosCancelacion2)
                .build();
        registroCancelacionAnualCita3 = RegistroCancelacionCitaAnual
                .builder()
                .mes("7")
                .totalCitasCanceladas(60)
                .registros(registrosCancelacion2)
                .build();
        registroCancelacionAnualCita4 = RegistroCancelacionCitaAnual
                .builder()
                .mes("2")
                .totalCitasCanceladas(60)
                .registros(registrosCancelacion2)
                .build();

        registroCancelacionMensualCita1 = RegistroCancelacionCitaMensual
                .builder()
                .dia("2")
                .totalCitasCanceladas(20)
                .registros(registrosCancelacion1)
                .build();
        registroCancelacionMensualCita2 = RegistroCancelacionCitaMensual
                .builder()
                .dia("12")
                .totalCitasCanceladas(40)
                .registros(registrosCancelacion2)
                .build();
        registroCancelacionMensualCita3 = RegistroCancelacionCitaMensual
                .builder()
                .dia("7")
                .totalCitasCanceladas(60)
                .registros(registrosCancelacion2)
                .build();
        registroCancelacionMensualCita4 = RegistroCancelacionCitaMensual
                .builder()
                .dia("2")
                .totalCitasCanceladas(60)
                .registros(registrosCancelacion2)
                .build();
    }
}
