package co.com.sura.datatest.maestros;

import co.com.sura.maestros.entity.HorarioTurno;

import java.time.LocalTime;

public class HorarioTurnoTestData {
    public HorarioTurno horarioTurnoT1;
    public HorarioTurno horarioTurnoT2;

    public HorarioTurnoTestData() {
       horarioTurnoT1= HorarioTurno
                .builder()
                .id(1)
                .nombre("T1")
                .descripcion("Mañana")
                .horaInicio(LocalTime.of(6,0))
                .horaFin(LocalTime.of(13,59))
                .duracionHoras(8)
                .esHorarioBase(true)
                .build();

        horarioTurnoT2= HorarioTurno
                .builder()
                .id(2)
                .nombre("T2")
                .descripcion("Tarde")
                .horaInicio(LocalTime.of(14,0))
                .horaFin(LocalTime.of(21,59))
                .duracionHoras(8)
                .esHorarioBase(true)
                .build();

    }
}
