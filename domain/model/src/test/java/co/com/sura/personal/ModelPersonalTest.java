package co.com.sura.personal;

import co.com.sura.datatest.maestros.HorarioTurnoTestData;
import co.com.sura.genericos.ResultadoActualizacionTurno;
import co.com.sura.maestros.entity.HorarioTurno;
import co.com.sura.personal.entity.SecuenciaTurno;
import co.com.sura.remision.entity.datosremision.ItemDiaTurno;
import co.com.sura.remision.entity.datosremision.ItemSecuenciaTurno;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

 class ModelPersonalTest {

    List<SecuenciaTurno> listSecuenciaTurno;
    HorarioTurnoTestData horarioTurnoTestData;

    @BeforeEach
    void setDataSecuencia(){
        horarioTurnoTestData = new HorarioTurnoTestData();
        List<HorarioTurno> horariosTurnoData1 = new ArrayList<>();
        horariosTurnoData1.add(horarioTurnoTestData.horarioTurnoT1);


        List<HorarioTurno> horariosTurnoData2 = new ArrayList<>();
        horariosTurnoData2.add(horarioTurnoTestData.horarioTurnoT2);

        listSecuenciaTurno = new ArrayList<>();

        List<ItemDiaTurno> itemsTurnoData1 = new ArrayList<>();
        List<ItemDiaTurno> itemsTurnoData2 = new ArrayList<>();

        itemsTurnoData1.add(ItemDiaTurno.builder()
                        .nombreDia("lunes")
                        .horariosTurno(horariosTurnoData1)
                        .numeroDia(1)
                .build());

        itemsTurnoData2.add(ItemDiaTurno.builder()
                .nombreDia("martes")
                .horariosTurno(horariosTurnoData2)
                .numeroDia(2)
                .build());

        listSecuenciaTurno.add(
                SecuenciaTurno.builder()
                        .nombre("ss1")
                        .descripcion("ds1")
                        .itemsDiaTurno(itemsTurnoData1)
                        .build()
        );
        listSecuenciaTurno.add(
                SecuenciaTurno.builder()
                        .nombre("ss1")
                        .descripcion("ds1")
                        .itemsDiaTurno(itemsTurnoData2)
                        .build()
        );
    }
    @Test
    void agruparItemsDiaTurno(){
        SecuenciaTurno secuenciaTurno = SecuenciaTurno.agruparItemsDiaTurno(listSecuenciaTurno);

        List<ItemDiaTurno> itemsTurno = new ArrayList<>();


        itemsTurno.add(ItemDiaTurno.builder()
                .nombreDia("lunes")
                .horariosTurno(new ArrayList<>(Collections.singleton(horarioTurnoTestData.horarioTurnoT1)))
                .numeroDia(1)
                .build());
        itemsTurno.add(ItemDiaTurno.builder()
                .nombreDia("martes")
                .horariosTurno(new ArrayList<>(Collections.singleton(horarioTurnoTestData.horarioTurnoT2)))
                .numeroDia(2)
                .build());

        SecuenciaTurno secuenciaTurnoExperada = SecuenciaTurno.builder()
                .nombre("ss1")
                .descripcion("ds1")
                .itemsDiaTurno(itemsTurno)
                .build();

        Assertions.assertNotNull(secuenciaTurno);
        Assertions.assertEquals(secuenciaTurnoExperada, secuenciaTurno);
    }
     @Test
     void crearSecuenciaTurno() {
         horarioTurnoTestData = new HorarioTurnoTestData();
         List<HorarioTurno> horariosTurnoData = new ArrayList<>();

         horariosTurnoData.add(horarioTurnoTestData.horarioTurnoT1);
         ItemSecuenciaTurno itemsSecuenciaTurno= ItemSecuenciaTurno.builder()
                 .idSecuencia(1)
                 .nombreSecuencia("ss1")
                 .descripcion("dd1")
                 .numeroDia(1)
                 .nombreDia("lunes")
                 .horariosTurno(horariosTurnoData)
                 .build();

         List<ItemDiaTurno> itemsTurno = new ArrayList<>();


         itemsTurno.add(ItemDiaTurno.builder()
                 .nombreDia("lunes")
                 .horariosTurno(new ArrayList<>(Collections.singleton(horarioTurnoTestData.horarioTurnoT1)))
                 .numeroDia(1)
                 .build());
         SecuenciaTurno secuenciaTurnoEsperada = SecuenciaTurno.builder()
                 .nombre("ss1")
                 .descripcion("dd1")
                 .itemsDiaTurno(itemsTurno)
                 .build();

         SecuenciaTurno secuenciaTurno = SecuenciaTurno.crearSecuenciaTurnoFromItemsSecuencia(itemsSecuenciaTurno);
         Assertions.assertEquals(secuenciaTurnoEsperada,secuenciaTurno);
    }

    @Test
     void ResultadoActualizacionNotNull(){
        ResultadoActualizacionTurno resultadoActualizacionTurnoNull = ResultadoActualizacionTurno.builder().build();
        ResultadoActualizacionTurno resultadoActualizacionTurnoNotNull = ResultadoActualizacionTurno.builder()
                .fechaTurno(LocalDate.now())
                .idProfesional("98989898")
                .mensaje("msg")
                .build();
        Assertions.assertFalse(ResultadoActualizacionTurno.isNotNull(resultadoActualizacionTurnoNull));
        Assertions.assertTrue(ResultadoActualizacionTurno.isNotNull(resultadoActualizacionTurnoNotNull));
    }

    @Test
     void iniciarListaTurnoVacio(){
        ItemDiaTurno itemDiaTurno = ItemDiaTurno.builder()
                .numeroDia(1)
                .nombreDia("Lunes")
                .horariosTurno(new ArrayList<>())
                .build();


        ItemDiaTurno itemDiaHorarioVacio = ItemDiaTurno.inicializarListaHorarioTurnoVacio(itemDiaTurno);
        Assertions.assertFalse(itemDiaHorarioVacio.getHorariosTurno().isEmpty());

    }
}
