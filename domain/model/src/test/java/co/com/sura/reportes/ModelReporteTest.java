package co.com.sura.reportes;

import co.com.sura.datatest.reportes.ReportesTestData;
import co.com.sura.reportes.entity.cancelacioncitas.*;
import co.com.sura.reportes.entity.turnos.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static co.com.sura.reportes.entity.cancelacioncitas.RegistroCancelacionCitaAnual.converNumeroToNombreMes;

class ModelReporteTest {
    ReportesTestData dataTest = new ReportesTestData();

    @Test
    void convertirNombreMes(){
        ItemReporteAnual itemReporteAnualNombreMes = ItemReporteAnual
                .converNumeroToNombreMes(dataTest.itemReporteAnualTest1);
        Assertions.assertEquals(itemReporteAnualNombreMes.getMes(), dataTest.nombreMesTest);
    }

    @Test
    void setNombreMes(){
        List<ItemReporteAnual> items = new ArrayList<>();
        items.add(dataTest.itemReporteAnualTest1);
        ReporteTurnoAnual reporte = ReporteTurnoAnual.builder()
                .reportes(items)
                .build();
        ReporteTurnoAnual itemReporteAnualNombreMes = ReporteTurnoAnual.setNombreMes(reporte).block();
        Assertions.assertNotNull(itemReporteAnualNombreMes);
        Assertions.assertEquals(itemReporteAnualNombreMes.getReportes().get(0).getMes(), dataTest.nombreMesTest);
    }

    @Test
    void calcularResumenReporteTurno(){
        List<ItemReporteAnual> items = new ArrayList<>();
        items.add(dataTest.itemReporteAnualTest1);
        items.add(dataTest.itemReporteAnualTest2);

        ResumenReporteTurno reporteAnualEsperado = ResumenReporteTurno
                .builder()
                .capacidad(55.0)
                .totalHoras(2000)
                .citasCompletadas(200)
                .citasCanceladas(20)
                .totalNovedades(400)
                .totalRemisiones(200)
                .cumplimiento(85.0)
                .build();
        ReporteTurnoAnual reporte = ReporteTurnoAnual.builder()
                .reportes(items)
                .build();
        ReporteTurnoAnual reporteTurnoAnual = ReporteTurnoAnual.calcularResumen(reporte).block();

        Assertions.assertNotNull(reporteTurnoAnual);
        Assertions.assertEquals(reporteTurnoAnual.getResumen(),reporteAnualEsperado);
    }

    @Test
    void calcularResumenRemorteMensual(){
        List<ItemReporteMensual> items = new ArrayList<>();
        items.add(dataTest.itemReporteMesualTest1);
        items.add(dataTest.itemReporteMensualTest2);

        ResumenReporteTurno reporteMensualEsperado = ResumenReporteTurno
                .builder()
                .capacidad(55.0)
                .totalHoras(2000)
                .citasCompletadas(200)
                .citasCanceladas(20)
                .totalNovedades(400)
                .totalRemisiones(200)
                .cumplimiento(85.0)
                .build();
        ReporteTurnoMensual reporte = ReporteTurnoMensual.builder()
                .reportes(items)
                .build();
        ReporteTurnoMensual reporteTurnoMensual = ReporteTurnoMensual.calcularResumen(reporte).block();

        Assertions.assertNotNull(reporteTurnoMensual);
        Assertions.assertEquals(reporteTurnoMensual.getResumen(),reporteMensualEsperado);
    }

    @Test
    void calcularTotalCantidadRegistroCancelacion(){
        Integer totalEsperado = 90;
        List<RegistroCancelacionCita> registros = new ArrayList<>();
        registros.add(new RegistroCancelacionCita("d1",20));
        registros.add(new RegistroCancelacionCita("d2",30));
        registros.add(new RegistroCancelacionCita("d3",40));
        Integer total = RegistroCancelacionCita.calcularTotalCantidad(registros);
        Assertions.assertEquals(total,totalEsperado);
    }

    @Test
    void orderarByMesRegistroCancelacionAnual(){


        List<RegistroCancelacionCitaAnual> registrosList= new ArrayList<>();
        registrosList.add(dataTest.registroCancelacionAnualCita1);
        registrosList.add(dataTest.registroCancelacionAnualCita2);
        registrosList.add(dataTest.registroCancelacionAnualCita3);

        List<RegistroCancelacionCitaAnual> registrosListEsperado= new ArrayList<>();
        registrosListEsperado.add(dataTest.registroCancelacionAnualCita1);
        registrosListEsperado.add(dataTest.registroCancelacionAnualCita3);
        registrosListEsperado.add(dataTest.registroCancelacionAnualCita2);



        RegistroCancelacionCitaAnual registroNombreMes = converNumeroToNombreMes(dataTest.registroValidarNombreMes);

        Assertions.assertEquals(registroNombreMes.getMes(), dataTest.mesTestEsperado);
        List<RegistroCancelacionCitaAnual> respuesta = RegistroCancelacionCitaAnual.ordenarListaByMes(registrosList);

         Assertions.assertArrayEquals(respuesta.toArray(), registrosListEsperado.toArray());

    }

    @Test
    void agruparReportesCancelacionCitaAnual(){
        List<RegistroCancelacionCitaAnual> registrosList= new ArrayList<>();
        registrosList.add(dataTest.registroCancelacionAnualCita1);
        registrosList.add(dataTest.registroCancelacionAnualCita4);
        registrosList.add(dataTest.registroCancelacionAnualCita3);
        ReporteCancelacionCitaAnual reporte = ReporteCancelacionCitaAnual
                .builder()
                .reportes(registrosList)
                .build();


        List<RegistroCancelacionCitaAnual> registrosListEsperado= new ArrayList<>();
        List<RegistroCancelacionCita> registroMes2 = dataTest.registrosCancelacion1;
        registroMes2.addAll(dataTest.registrosCancelacion2);
        registrosListEsperado.add( RegistroCancelacionCitaAnual
                .builder()
                .mes("2")
                .totalCitasCanceladas(40)
                .registros(registroMes2)
                .build());

       registrosListEsperado.add(RegistroCancelacionCitaAnual
               .builder()
               .mes("7")
               .totalCitasCanceladas(20)
               .registros(dataTest.registrosCancelacion2)
               .build());

        registrosListEsperado = RegistroCancelacionCitaAnual.ordenarListaByMes((registrosListEsperado));

        ReporteCancelacionCitaAnual reporteEsperado = ReporteCancelacionCitaAnual
                .builder()
                .resumen(ResumenCancelacionCita.builder()
                        .totalCancelaciones(60)
                        .registrosCancelacion(registroMes2)
                        .build())
                .reportes(registrosListEsperado)
                .build();

        ReporteCancelacionCitaAnual reporteAgrupado = ReporteCancelacionCitaAnual.agruparReportes(reporte).block();
        ReporteCancelacionCitaAnual reporteResumen= ReporteCancelacionCitaAnual.calcularResumen(reporte).block();

        Assertions.assertNotNull(reporteAgrupado);
        Assertions.assertNotNull(reporteResumen);

        Assertions.assertEquals(
                reporteEsperado.getReportes().toString(),
                reporteAgrupado.getReportes().toString());

        Assertions.assertEquals(
                reporteEsperado.getResumen(),
                reporteResumen.getResumen());
    }

    @Test
    void agruparReportesCancelacionCitaMensual(){
        List<RegistroCancelacionCitaMensual> registrosList= new ArrayList<>();
        registrosList.add(dataTest.registroCancelacionMensualCita1);
        registrosList.add(dataTest.registroCancelacionMensualCita4);
        registrosList.add(dataTest.registroCancelacionMensualCita3);
        ReporteCancelacionCitaMensual reporte = ReporteCancelacionCitaMensual
                .builder()
                .reportes(registrosList)
                .build();


        List<RegistroCancelacionCitaMensual> registrosListEsperado= new ArrayList<>();
        List<RegistroCancelacionCita> registroMes2 = dataTest.registrosCancelacion1;
        registroMes2.addAll(dataTest.registrosCancelacion2);
        registrosListEsperado.add( RegistroCancelacionCitaMensual
                .builder()
                .dia("2")
                .totalCitasCanceladas(40)
                .registros(registroMes2)
                .build());

        registrosListEsperado.add(RegistroCancelacionCitaMensual
                .builder()
                .dia("7")
                .totalCitasCanceladas(20)
                .registros(dataTest.registrosCancelacion2)
                .build());


        ReporteCancelacionCitaMensual reporteEsperado = ReporteCancelacionCitaMensual
                .builder()
                .resumen(ResumenCancelacionCita.builder()
                        .totalCancelaciones(60)
                        .registrosCancelacion(registroMes2)
                        .build())
                .reportes(registrosListEsperado)
                .build();

        ReporteCancelacionCitaMensual reporteAgrupado = ReporteCancelacionCitaMensual.agruparReportes(reporte).block();
        ReporteCancelacionCitaMensual reporteResumen= ReporteCancelacionCitaMensual.calcularResumen(reporte).block();

        Assertions.assertNotNull(reporteAgrupado);
        Assertions.assertNotNull(reporteResumen);

        Assertions.assertEquals(
                reporteEsperado.getReportes().toString(),
                reporteAgrupado.getReportes().toString());

        Assertions.assertEquals(
                reporteEsperado.getResumen(),
                reporteResumen.getResumen());
    }
}
