package co.com.sura.reportes;

import co.com.sura.maestros.entity.Regional;
import co.com.sura.postgres.maestros.data.RegionalData;
import co.com.sura.postgres.maestros.repository.RegionalesRepository;
import co.com.sura.postgres.reportes.adapter.ReportesAdapter;
import co.com.sura.reportes.entity.cancelacioncitas.*;
import co.com.sura.reportes.entity.turnos.ItemReporteAnual;
import co.com.sura.reportes.entity.turnos.ItemReporteMensual;
import co.com.sura.reportes.entity.turnos.ReporteTurnoAnual;
import co.com.sura.reportes.entity.turnos.ReporteTurnoMensual;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.transaction.reactive.TransactionalOperator;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Collections;

@ExtendWith(MockitoExtension.class)
class ReporteRepositoryAdapterTest {

    private final Integer anio = 2023;
    private final String idRegional = "427";
    private final Integer numeroMes = 1;

    private  RegionalData regionalData;
    private  Regional regional;
    @Mock
    private  RegionalesRepository regionalesRepositoryMock;
    @Mock
    private  R2dbcEntityTemplate r2dbcEntityTemplateMock;
    @Mock
    private  TransactionalOperator transactionalOperatorMock;
    @InjectMocks
    private ReportesAdapter reportesAdapter;
    @BeforeEach
    void setUpData(){
        this.regionalData =  RegionalData.builder()
                .id(this.idRegional)
                .build();

        this.regional =  Regional.builder()
                .id(this.idRegional)
                .build();
    }

    @Test
    void consultarReporteAnual(){

        //data
        ItemReporteAnual itemReporteAnual =    ItemReporteAnual.builder()
                .mes("1")
                .totalRemisiones(1)
                .cumplimientoCitasPromedio(1.0)
                .totalNovedades(1)
                .totalCitasCompletadas(1)
                .capacidadPromedio(1.0)
                .totalCitasCanceladas(1)
                .totalHorasAtencionesCompletadas(1.0)
                .build();

        ReporteTurnoAnual reporteTurnoAnual = ReporteTurnoAnual.builder()
                .regional(this.regional)
                .reportes(Collections.singletonList(itemReporteAnual))
                        .build();

        Mockito.when(regionalesRepositoryMock.findById(this.idRegional))
                .thenReturn(Mono.just(this.regionalData));

        Mockito.when(transactionalOperatorMock.execute(Mockito.any()))
                .thenAnswer(invocacion -> Flux.fromIterable(Collections.singletonList(itemReporteAnual)));

        Mono<ReporteTurnoAnual> response = reportesAdapter.consultarReporteAnual(anio,idRegional);

        StepVerifier.create(response)
                .expectNext(reporteTurnoAnual)
                .verifyComplete();
    }

    @Test
    void consultarReporteMensual(){

        //data
        ItemReporteMensual itemReporteMensual =    ItemReporteMensual.builder()
                .dia("1")
                .totalRemisiones(1)
                .cumplimientoCitasPromedio(1.0)
                .totalNovedades(1)
                .totalCitasCompletadas(1)
                .capacidadPromedio(1.0)
                .totalCitasCanceladas(1)
                .totalHorasAtencionesCompletadas(1.0)
                .build();

        ReporteTurnoMensual reporteTurnoMensual = ReporteTurnoMensual.builder()
                .regional(this.regional)
                .reportes(Collections.singletonList(itemReporteMensual))
                .build();

        Mockito.when(regionalesRepositoryMock.findById(this.idRegional))
                .thenReturn(Mono.just(this.regionalData));

        Mockito.when(transactionalOperatorMock.execute(Mockito.any()))
                .thenAnswer(invocacion -> Flux.fromIterable(Collections.singletonList(itemReporteMensual)));

        Mono<ReporteTurnoMensual> response = reportesAdapter.consultarReporteMensual(anio,numeroMes,idRegional);

        StepVerifier.create(response)
                .expectNext(reporteTurnoMensual)
                .verifyComplete();
    }
    @Test
    void consultaReporteCancelacionCitasAnual(){

        //data
        RegistroCancelacionCita registroCancelacionCita = RegistroCancelacionCita.builder()
                .descripcion("descripcion")
                .cantidad(1)
                .build();

        RegistroCancelacionCitaAnual registroCancelacionCitaAnual = RegistroCancelacionCitaAnual.builder()
                .mes("1")
                .totalCitasCanceladas(1)
                .registros(Collections.singletonList(registroCancelacionCita))
                .build();

        ReporteCancelacionCitaAnual reporteCancelacionCitaAnual = ReporteCancelacionCitaAnual.builder()
                .regional(this.regional)
                .reportes(Collections.singletonList(registroCancelacionCitaAnual.toBuilder().mes("enero").build()))
                .resumen(ResumenCancelacionCita.builder()
                        .totalCancelaciones(1)
                        .registrosCancelacion(Collections.singletonList(registroCancelacionCita))
                        .build())
                .build();

        Mockito.when(regionalesRepositoryMock.findById(this.idRegional))
                .thenReturn(Mono.just(this.regionalData));

        Mockito.when(transactionalOperatorMock.execute(Mockito.any()))
                .thenAnswer(invocacion -> Flux.fromIterable(Collections.singletonList(registroCancelacionCitaAnual)));

        ReporteCancelacionCitaAnual response = reportesAdapter
                .consultaReporteCancelacionCitasAnual(anio,idRegional)
                .block();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(reporteCancelacionCitaAnual.toString(), response.toString());
    }
    @Test
    void consultaReporteCancelacionCitasMensual(){

        //data
        RegistroCancelacionCita registroCancelacionCita = RegistroCancelacionCita.builder()
                .descripcion("descripcion")
                .cantidad(1)
                .build();

        RegistroCancelacionCitaMensual registroCancelacionCitaMensual = RegistroCancelacionCitaMensual.builder()
                .dia("1")
                .totalCitasCanceladas(1)
                .registros(Collections.singletonList(registroCancelacionCita))
                .build();

        ReporteCancelacionCitaMensual reporteCancelacionCitaMensual = ReporteCancelacionCitaMensual.builder()
                .regional(this.regional)
                .reportes(Collections.singletonList(registroCancelacionCitaMensual))
                .resumen(ResumenCancelacionCita.builder()
                        .totalCancelaciones(1)
                        .registrosCancelacion(Collections.singletonList(registroCancelacionCita))
                        .build())
                .build();

        Mockito.when(regionalesRepositoryMock.findById(this.idRegional))
                .thenReturn(Mono.just(this.regionalData));

        Mockito.when(transactionalOperatorMock.execute(Mockito.any()))
                .thenAnswer(invocacion -> Flux.fromIterable(Collections.singletonList(registroCancelacionCitaMensual)));

        ReporteCancelacionCitaMensual response = reportesAdapter
                .consultaReporteCancelacionCitasMensual(anio,numeroMes,idRegional)
                .block();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(reporteCancelacionCitaMensual.toString(), response.toString());
    }

}