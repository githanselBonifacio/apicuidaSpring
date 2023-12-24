package co.com.sura.reportes;

import co.com.sura.postgres.maestros.data.RegionalesData;
import co.com.sura.postgres.maestros.repository.RegionalesRepository;
import co.com.sura.postgres.reportes.adapter.ReportesAdapter;
import co.com.sura.reportes.entity.turnos.ReporteTurnoAnual;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.r2dbc.core.DatabaseClient;
import org.springframework.transaction.reactive.TransactionalOperator;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.ArgumentMatchers.anyString;

@ExtendWith(MockitoExtension.class)
class ReporteRepositoryAdapterTest {

    private final Integer anio = 2023;
    private final String idRegional = "427";
    private final Integer numeroMes = 1;

    @Mock
    private  RegionalesRepository regionalesRepositoryMock;
    @Mock
    private  R2dbcEntityTemplate r2dbcEntityTemplateMock;
    @Mock
    private  TransactionalOperator transactionalOperatorMock;

    @InjectMocks
    private ReportesAdapter reportesAdapter;

    @Test
    void consultarReporteAnual(){

        Mockito.when(regionalesRepositoryMock.findById(idRegional))
                .thenReturn(Mono.just(
                        RegionalesData.builder()
                                .id(idRegional)
                                .build()
                ));

        Mono<ReporteTurnoAnual> response = reportesAdapter.consultarReporteAnual(anio,idRegional);

        StepVerifier.create(response)
                .expectNext(ReporteTurnoAnual.builder().build())
                .expectComplete();
    }
}
