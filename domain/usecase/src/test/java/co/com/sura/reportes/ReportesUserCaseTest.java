package co.com.sura.reportes;

import co.com.sura.personal.entity.Profesional;
import co.com.sura.reportes.entity.cancelacioncitas.ReporteCancelacionCitaAnual;
import co.com.sura.reportes.entity.cancelacioncitas.ReporteCancelacionCitaMensual;
import co.com.sura.reportes.entity.turnos.ReporteTurnoAnual;
import co.com.sura.reportes.entity.turnos.ReporteTurnoMensual;
import co.com.sura.reportes.gateway.ReportesRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

@ExtendWith(MockitoExtension.class)
 class ReportesUserCaseTest {
    private final Integer anio = 2023;
    private final String idRegional = "427";
    private final Integer numeroMes = 2;

    @Mock
    private ReportesRepository reportesRepositoryMock;

    @InjectMocks
    private ReportesUseCase reportesUseCaseMock;

    @Test
    void consultarReporteAnual(){
       ReporteTurnoAnual reporteTurnoAnual = ReporteTurnoAnual.builder().build();

       Mockito.when(reportesRepositoryMock.consultarReporteAnual(anio,idRegional))
               .thenReturn(Mono.just(reporteTurnoAnual));

       Mono<ReporteTurnoAnual> consultaReporteAnual = reportesUseCaseMock
               .consultarReporteAnual(anio,idRegional);

       StepVerifier.create(consultaReporteAnual)
               .expectNext(reporteTurnoAnual)
               .expectComplete()
               .verify();
    }
    @Test
    void consultarReporteMes(){
       ReporteTurnoMensual reporteTurnoMesual = ReporteTurnoMensual.builder().build();

       Mockito.when(reportesRepositoryMock.consultarReporteMensual(anio,numeroMes,idRegional))
               .thenReturn(Mono.just(reporteTurnoMesual));

       Mono<ReporteTurnoMensual> consultaReporteMesual = reportesUseCaseMock
               .consultarReporteMes(anio,numeroMes,idRegional);

       StepVerifier.create(consultaReporteMesual)
               .expectNext(reporteTurnoMesual)
               .expectComplete()
               .verify();
    }
    @Test
    void consultaReporteCancelacionCitasAnual(){
       ReporteCancelacionCitaAnual reporteCancelacionCitaAnual = ReporteCancelacionCitaAnual.builder().build();

       Mockito.when(reportesRepositoryMock.consultaReporteCancelacionCitasAnual(anio,idRegional))
               .thenReturn(Mono.just(reporteCancelacionCitaAnual));

       Mono<ReporteCancelacionCitaAnual> consultaReporteCancelacionanual = reportesUseCaseMock
               .consultaReporteCancelacionCitasAnual(anio,idRegional);

       StepVerifier.create(consultaReporteCancelacionanual)
               .expectNext(reporteCancelacionCitaAnual)
               .expectComplete()
               .verify();
    }
    @Test
    void consultaReporteCancelacionCitasMensual(){
       ReporteCancelacionCitaMensual reporteCancelacionCitaMesual = ReporteCancelacionCitaMensual.builder().build();

       Mockito.when(reportesRepositoryMock.consultaReporteCancelacionCitasMensual(anio,numeroMes,idRegional))
               .thenReturn(Mono.just(reporteCancelacionCitaMesual));

       Mono<ReporteCancelacionCitaMensual> consultaReporteCancelacionMensual = reportesUseCaseMock
               .consultaReporteCancelacionCitasMensual(anio,numeroMes,idRegional);

       StepVerifier.create(consultaReporteCancelacionMensual)
               .expectNext(reporteCancelacionCitaMesual)
               .expectComplete()
               .verify();
    }
}
