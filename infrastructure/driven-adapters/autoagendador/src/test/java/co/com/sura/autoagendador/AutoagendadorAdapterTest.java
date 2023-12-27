package co.com.sura.autoagendador;

import co.com.sura.autoagendador.models.AutoAgendador;
import co.com.sura.autoagendador.models.CitaGenetic;
import co.com.sura.autoagendador.models.Resultado;
import co.com.sura.mapbox.gateway.MapboxServiceRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
public class AutoagendadorAdapterTest {
    public static final Integer NUMERO_GENERACIONES           = 5;
    public static final Integer SIZE_POBLACION_INICIAL        = 2;
    public static final Integer NUMERO_PADRES_EMPAREJADOS     = 2;
    public static final double  PENALIZACION_HOLGURA_NEGATIVA = 1e6;

    @Mock
    private MapboxServiceRepository mapboxServiceRepositoryMock;
    private AutoAgendador autoAgendador;

    @BeforeEach
    void setUp(){
        autoAgendador = new AutoAgendador(
                NUMERO_GENERACIONES,
                SIZE_POBLACION_INICIAL,
                NUMERO_PADRES_EMPAREJADOS,
                PENALIZACION_HOLGURA_NEGATIVA,
                mapboxServiceRepositoryMock);
    }

    @Test
    void autoagendar(){
        LocalDate fechaProgramada = LocalDate.now();
        Integer duracion = 600;
        CitaGenetic origen = CitaGenetic.builder()
                .idCita("0")
                .latitud(11.8)
                .longitud(-7.8)
                .duracion(duracion)
                .fechaInicioIso(fechaProgramada.atTime(LocalTime.of(6,0))
                        .toEpochSecond(ZoneOffset.UTC))
                .build();

        List<CitaGenetic> citaGeneticList = new ArrayList<>();
        citaGeneticList.add(
                CitaGenetic.builder()
                        .idCita("1")
                        .latitud(11.2)
                        .longitud(-7.2)
                        .duracion(duracion)
                        .fechaInicioIso(fechaProgramada.atTime(LocalTime.of(8,0))
                                .toEpochSecond(ZoneOffset.UTC))
                        .build());

        citaGeneticList.add(
                CitaGenetic.builder()
                        .idCita("2")
                        .latitud(11.5)
                        .longitud(-7.5)
                        .duracion(duracion)
                        .fechaInicioIso(fechaProgramada.atTime(LocalTime.of(9,0))
                                .toEpochSecond(ZoneOffset.UTC))
                        .build());

        citaGeneticList.add(
                CitaGenetic.builder()
                        .idCita("3")
                        .latitud(11.1)
                        .longitud(-7.1)
                        .duracion(duracion)
                        .fechaInicioIso(fechaProgramada.atTime(LocalTime.of(10,0))
                                .toEpochSecond(ZoneOffset.UTC))
                        .build());

        citaGeneticList.add(
                CitaGenetic.builder()
                        .idCita("4")
                        .latitud(11.9)
                        .longitud(-7.9)
                        .duracion(duracion)
                        .fechaInicioIso(fechaProgramada.atTime(LocalTime.of(10,0))
                                .toEpochSecond(ZoneOffset.UTC))
                        .build());

        citaGeneticList.add(
                CitaGenetic.builder()
                        .idCita("5")
                        .latitud(11.05)
                        .longitud(-7.06)
                        .duracion(duracion)
                        .fechaInicioIso(fechaProgramada.atTime(LocalTime.of(10,0))
                                .toEpochSecond(ZoneOffset.UTC))
                        .build());
        Integer numeroMoviles = 2;

        autoAgendador.withCitas(citaGeneticList)
                .andOrigen(origen)
                .andNumeroMoviles(numeroMoviles);

        Resultado resultado = autoAgendador.run();
        Assertions.assertNotNull(resultado.getIndividuo());

        autoAgendador.resetData();
        Assertions.assertTrue(autoAgendador.getCitas().isEmpty());

    }
}
