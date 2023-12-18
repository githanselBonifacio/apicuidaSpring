package co.com.sura.farmacia;

import co.com.sura.agenda.entity.PacienteTratamientoCita;
import co.com.sura.farmacia.gateway.FarmaciaRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class FarmaciaUserCaseTest {
    private final LocalDate fechaTurno = LocalDate.of(2023,7,7);
    @Mock
    private FarmaciaRepository farmaciaRepositoryMock;

    @InjectMocks
    private FarmaciaUseCase farmaciaUseCaseMock;

    @Test
    void consultarAllTratamientosToFarmacia(){
        List<PacienteTratamientoCita> pacienteTratamientoCitas = new ArrayList<>();
        pacienteTratamientoCitas.add(PacienteTratamientoCita.builder().build());
        pacienteTratamientoCitas.add(PacienteTratamientoCita.builder().build());

        Mockito.when(farmaciaRepositoryMock.consultarAllPacienteWithMedicamentosToFarmacia())
                .thenReturn(Flux.fromIterable(pacienteTratamientoCitas));

        Flux<PacienteTratamientoCita> consultaTratamientos = farmaciaUseCaseMock
                .consultarAllTratamientosToFarmacia();

        StepVerifier.create(consultaTratamientos)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarAllTratamientosToFarmaciaWithFilter(){
        List<PacienteTratamientoCita> pacienteTratamientoCitas = new ArrayList<>();
        pacienteTratamientoCitas.add(PacienteTratamientoCita.builder().build());
        pacienteTratamientoCitas.add(PacienteTratamientoCita.builder().build());

        String idRegional = "427";
        Integer idHorarioTurno = 1;
        Mockito.when(farmaciaRepositoryMock.consultarAllPacienteWithMedicamentosToFarmaciaByFilter(
                        fechaTurno, idHorarioTurno, idRegional))
                .thenReturn(Flux.fromIterable(pacienteTratamientoCitas));

        Flux<PacienteTratamientoCita> consultaTratamientos = farmaciaUseCaseMock
                .consultarAllTratamientosToFarmaciaWithFilter( fechaTurno, idHorarioTurno, idRegional);

        StepVerifier.create(consultaTratamientos)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void notificarMedicamentosToFarmacia(){
        List<PacienteTratamientoCita> pacienteTratamientoCitas = new ArrayList<>();
        pacienteTratamientoCitas.add(PacienteTratamientoCita.builder().build());
        pacienteTratamientoCitas.add(PacienteTratamientoCita.builder().build());

        Mockito.when(farmaciaRepositoryMock.notificarMedicamentosToFarmacia(pacienteTratamientoCitas))
                .thenReturn(Mono.just(true));

        Mono<Boolean> seNotificoTratamientos = farmaciaUseCaseMock
                .notificarMedicamentosToFarmacia( pacienteTratamientoCitas);

        StepVerifier.create(seNotificoTratamientos)
                .expectNext(true)
                .expectComplete()
                .verify();
    }
}
